module HandBrake.HB
  ( main )
where

-- base --------------------------------

import Control.Monad       ( MonadFail, filterM, forM, forM_, return, when )
import Data.Eq             ( Eq )
import Data.Function       ( ($), (&) )
import Data.List           ( nub, tails )
import Data.List.NonEmpty  ( NonEmpty, toList )
import Data.Maybe          ( maybe )
import Data.Ord            ( (>) )
import Data.Tuple          ( snd )
import Data.Word           ( Word8 )
import GHC.Stack           ( HasCallStack )
import System.IO           ( IO )
import Text.Show           ( show )

-- base-unicode-symbols ----------------

import Data.Bool.Unicode      ( (∨) )
import Data.Eq.Unicode        ( (≡), (≢) )
import Data.Function.Unicode  ( (∘) )

-- containers-plus ---------------------

import ContainersPlus.Member  ( (∈) )

-- data-textual ------------------------

import Data.Textual  ( Printable, toText )

-- fpath -------------------------------

import FPath.AbsDir            ( AbsDir )
import FPath.AbsFile           ( AbsFile )
import FPath.Error.FPathError  ( AsFPathError, FPathIOError )
import FPath.File              ( File )

-- lens --------------------------------

import Control.Lens.Tuple  ( _2 )

-- log-plus ----------------------------

import Log  ( Log, warn )

-- logging-effect ----------------------

import Control.Monad.Log  ( LoggingT, MonadLog )

-- mockio ------------------------------

import MockIO  ( DoMock( DoMock, NoMock ) )

-- mockio-log --------------------------

import MockIO.IOClass      ( IOClass( NoIO ) )
import MockIO.Log          ( errIO' )
import MockIO.MockIOClass  ( MockIOClass( MockIOClass ) )

-- mockio-plus -------------------------

import MockIO.Process            ( (!) )
import MockIO.Process.MLCmdSpec  ( mkMLCmdW' )

-- monaderror-io -----------------------

import MonadError.IO        ( ӝ )
import MonadError.IO.Error  ( AsIOError )

-- monadio-plus ------------------------

import MonadIO                        ( MonadIO )
import MonadIO.Error.CreateProcError  ( AsCreateProcError )
import MonadIO.Error.ProcExitError    ( AsProcExitError )
import MonadIO.File                   ( devnull )
import MonadIO.FPath                  ( getCwd, pResolve )
import MonadIO.FStat                  ( FExists( FExists, NoFExists ), fexists )
import MonadIO.Process.CmdSpec        ( cwd )
import MonadIO.Process.ExitStatus     ( ExitStatus( ExitVal ) )

-- more-unicode ------------------------

import Data.MoreUnicode.Bool     ( 𝔹 )
import Data.MoreUnicode.Functor  ( (⊳), (⩺) )
import Data.MoreUnicode.Lens     ( (⊣), (⊢) )
import Data.MoreUnicode.Maybe    ( 𝕄, pattern 𝕵 )
import Data.MoreUnicode.Monad    ( (≫) )
import Data.MoreUnicode.Text     ( 𝕋 )

-- mtl ---------------------------------

import Control.Monad.Except  ( ExceptT, MonadError )

-- natural -----------------------------

import Natural  ( ℕ, One, count, length )

-- stdmain -----------------------------

import StdMain             ( stdMain'' )
import StdMain.StdOptions  ( DryRunLevel )
import StdMain.UsageError  ( AsUsageError, UsageFPProcIOError, throwUsage )

-- text --------------------------------

import Data.Text  ( pack )

-- tfmt --------------------------------

import Text.Fmt  ( fmtT )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified HandBrake.Paths  as  Paths

import HandBrake.Encode   ( EncodeDetails, details, encodeRequest1, encodeArgs )
import HandBrake.Options  ( Options( Encode, Scan ), Overwrite( Overwrite )
                          , parseOptions )

--------------------------------------------------------------------------------

scan ∷ ∀ ε μ .
       (MonadIO μ, MonadError ε μ, Printable ε,
        AsIOError ε, AsFPathError ε, AsCreateProcError ε, AsProcExitError ε,
        MonadLog (Log MockIOClass) μ) ⇒
       File → AbsDir → 𝕄 ℕ → DoMock → μ ()
scan file wd stream_id do_mock = do
  let args = [ "--scan"
             , "--input", toText file
             , "--title", maybe "0" (pack ∘ show) stream_id
             ]
      cmd  = mkMLCmdW' Paths.handbrakeCLI args do_mock & cwd ⊢ 𝕵 wd
  snd ⊳ (devnull ≫ \null → null ! cmd)

----------------------------------------

encArgs ∷ ∀ ε μ .
       (MonadIO μ,
        AsUsageError ε, AsIOError ε, AsFPathError ε, AsCreateProcError ε,
        AsProcExitError ε,
        MonadError ε μ, Printable ε,
        MonadLog (Log MockIOClass) μ) ⇒
       AbsFile → AbsDir → ℕ → 𝕋 → EncodeDetails → DoMock → μ ([𝕋], AbsFile)

encArgs file output_dir stream_id name ds do_mock = do
  fexists output_dir ≫ \ x → when (x ≡ NoFExists) $
    let msg = [fmtT|No such output dir: %T|] output_dir
     in case do_mock of
          DoMock → warn (MockIOClass NoIO NoMock) msg
          NoMock → throwUsage msg
  let req = encodeRequest1 file output_dir stream_id (𝕵 name) & details ⊢ ds
  encodeArgs req

----------------------------------------

{- | Execute an encode with given args, with stdin connected to devnull; return
     0 in case of success (throw an exception otherwise). -}
do_encode ∷ ∀ ε μ .
            (MonadIO μ, MonadFail μ,
             AsUsageError ε, AsIOError ε, AsFPathError ε, AsCreateProcError ε,
             AsProcExitError ε,
             MonadError ε μ, Printable ε,
             MonadLog (Log MockIOClass) μ) ⇒
            [𝕋] → DoMock → μ ()
do_encode args do_mock = do
  let cmd  = mkMLCmdW' Paths.handbrakeCLI args do_mock
  (ExitVal 0,()) ← devnull ≫ \ null → null ! cmd
  return ()

----------------------------------------

{- | Very cheap duplicate detector.  Each element e in the input list will be
     cited (n-1) times in the output, where n is the number of occurences of e
     in the input list.
 -}
duplicates ∷ Eq α ⇒ [α] → [α]
duplicates xs = [ y | x ← tails xs, length x > 1, let (y:ys) = x, y ∈ ys]

{- | Check that a list of files contains no duplicates, and no extant files.
     The `overwrite` argument, if true, skips the extant files check.
     Errors will be logged (at `Error` level); and an exception will be thrown
     unless `do_mock` is `DoMock`.
-}
checkOutputFiles ∷ (MonadIO μ, MonadLog (Log MockIOClass) μ,
                    AsIOError ε, AsUsageError ε, MonadError ε μ) ⇒
                   [AbsFile] → 𝔹 → DoMock → μ ()
checkOutputFiles fns overwrite do_mock = do
  let dups = duplicates fns
  extants ← if overwrite
            then return []
            else filterM ((≡ FExists) ⩺ fexists) (nub fns)

  when (dups ≢ [] ∨ extants ≢ []) $ do
    forM_ dups (\ d → errIO' $ [fmtT|duplicate output: %T|] d)
    forM_ extants (\ e → errIO' $ [fmtT|output file already exists: %T|] e)
    when (do_mock ≡ NoMock) $
      throwUsage @𝕋 "duplicate or extant output files found"

----------------------------------------

{- | Encode multiple titles from a single input file. -}
encodes ∷ ∀ ε μ .
          (MonadIO μ, MonadFail μ, Printable ε, MonadError ε μ,
           AsUsageError ε, AsIOError ε, AsFPathError ε, AsCreateProcError ε,
           AsProcExitError ε,
           MonadLog (Log MockIOClass) μ) ⇒
          AbsFile → AbsDir → NonEmpty (ℕ,𝕋) → EncodeDetails → Overwrite → DoMock
        → μ ()
encodes input output_dir ts ds ov do_mock = do
  let enc_args (stream_id,name) =
        encArgs input output_dir stream_id name ds do_mock
  encFnArgs ∷ NonEmpty ([𝕋],AbsFile) ← forM ts enc_args
  checkOutputFiles (toList $ (⊣ _2) ⊳ encFnArgs) (Overwrite ≡ ov) do_mock
  forM_ (toList encFnArgs) (\ (args,_) → do_encode args do_mock )

----------------------------------------

myMain ∷ ∀ ε .
         (HasCallStack, Printable ε, AsUsageError ε,
          AsIOError ε, AsProcExitError ε, AsCreateProcError ε, AsFPathError ε) ⇒
         AbsDir → DryRunLevel One → Options
       → LoggingT (Log MockIOClass) (ExceptT ε IO) Word8
myMain wd dry_run opts = do
  let do_mock = if 0 ≢ count dry_run then DoMock else NoMock
  case opts of
    Scan   f n          → scan f wd n do_mock
    Encode f ts ds d ov → do output_dir ← pResolve d
                             f'         ← pResolve f
                             encodes f' output_dir ts ds ov do_mock
  return 0

----------------------------------------

main ∷ IO ()
main = do
  let progDesc = "HandBrakeCLI wrapper"
  wd ← ӝ (getCwd @FPathIOError)
  stdMain'' progDesc (parseOptions wd) (myMain @UsageFPProcIOError wd)

-- that's all, folks! ----------------------------------------------------------
