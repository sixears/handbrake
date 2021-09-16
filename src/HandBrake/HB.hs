module HandBrake.HB
  ( main )
where

-- base --------------------------------

import Control.Monad       ( forM, return )
import Data.Function       ( (&) )
import Data.List.NonEmpty  ( NonEmpty, toList, unzip )
import Data.Maybe          ( maybe )
import Data.Word           ( Word8 )
import GHC.Stack           ( HasCallStack )
import System.IO           ( IO )
import Text.Show           ( show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable, toText )

-- fpath -------------------------------

import FPath.AbsDir            ( AbsDir )
import FPath.AbsFile           ( AbsFile )
import FPath.Error.FPathError  ( AsFPathError, FPathIOError )
import FPath.File              ( File )

-- log-plus ----------------------------

import Log  ( Log )

-- logging-effect ----------------------

import Control.Monad.Log  ( LoggingT )

-- mockio ------------------------------

import MockIO  ( DoMock )

-- mockio-log --------------------------

import MockIO.MockIOClass  ( MockIOClass )

-- mockio-plus -------------------------

import MockIO.Process.MLCmdSpec  ( MLCmdSpec, mkMLCmdW' )

-- monaderror-io -----------------------

import MonadError.IO        ( ӝ )
import MonadError.IO.Error  ( AsIOError )

-- monadio-plus ------------------------

import MonadIO.Base                   ( getArgs )
import MonadIO.Error.CreateProcError  ( AsCreateProcError )
import MonadIO.Error.ProcExitError    ( AsProcExitError )
import MonadIO.FPath                  ( getCwd, pResolve )
import MonadIO.Process.CmdSpec        ( cwd )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor  ( (⊳) )
import Data.MoreUnicode.Lens     ( (⊢) )
import Data.MoreUnicode.Maybe    ( 𝕄, pattern 𝕵 )
import Data.MoreUnicode.Monad    ( (≫) )
import Data.MoreUnicode.Text     ( 𝕋 )

-- mtl ---------------------------------

import Control.Monad.Except  ( ExceptT, MonadError )

-- natural -----------------------------

import Natural  ( ℕ )

-- stdmain -----------------------------

import StdMain             ( Overwrite( NoOverwrite )
                           , checkRunNICmds', stdMain )
import StdMain.UsageError  ( AsUsageError, UsageFPProcIOError )

-- text --------------------------------

import Data.Text  ( pack )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified HandBrake.Paths  as  Paths

import HandBrake.Encode   ( EncodeDetails, EncodeRequest
                          , details, encodeRequest1, encodeArgs )
import HandBrake.Options  ( Options( Encode, Scan )
                          , parseOptions )

--------------------------------------------------------------------------------

scan ∷ File → AbsDir → 𝕄 ℕ → DoMock → MLCmdSpec()
scan file wd stream_id do_mock =
  let args = [ "--scan"
             , "--input", toText file
             , "--title", maybe "0" (pack ∘ show) stream_id
             ]
   in mkMLCmdW' Paths.handbrakeCLI args do_mock & cwd ⊢ 𝕵 wd

----------------------------------------

encArgs ∷ ∀ ε η . (AsUsageError ε, AsFPathError ε, MonadError ε η) ⇒
           AbsFile → AbsDir → ℕ → 𝕋 → EncodeDetails
         → η (DoMock → MLCmdSpec (),AbsFile)

encArgs file output_dir stream_id name ds = do
  let req ∷ EncodeRequest
      req = encodeRequest1 file output_dir stream_id (𝕵 name) & details ⊢ ds
  (args, outputf) ← encodeArgs req
  return (mkMLCmdW' Paths.handbrakeCLI args, outputf)

----------------------------------------

{- | Encode multiple titles from a single input file. -}
encodes ∷ ∀ ε μ . (MonadError ε μ, AsUsageError ε, AsFPathError ε) ⇒
          AbsFile → AbsDir → NonEmpty (ℕ,𝕋) → EncodeDetails
        → μ ([DoMock → MLCmdSpec ()], [AbsFile], [AbsDir])
encodes input output_dir ts ds = do
  let enc_args ∷ (ℕ,𝕋) → μ (DoMock → MLCmdSpec (), AbsFile)
      enc_args (stream_id,name) = do
        (mkCmd,outputf) ← encArgs input output_dir stream_id name ds
        return (mkCmd,outputf)

  (cmds,fns) ∷ (NonEmpty (DoMock → MLCmdSpec ()), NonEmpty AbsFile)
             ← unzip ⊳ forM ts enc_args
  return ((toList cmds),(toList fns),[output_dir])

----------------------------------------

myMain ∷ ∀ ε .
         (HasCallStack, Printable ε, AsUsageError ε,
          AsIOError ε, AsProcExitError ε, AsCreateProcError ε, AsFPathError ε) ⇒
         AbsDir → DoMock → Options
       → LoggingT (Log MockIOClass) (ExceptT ε IO) Word8
myMain wd do_mock opts = do
  (overwrite,cmds,fns,dirs) ← case opts of
    Scan   f n          → let cmd = scan f wd n
                           in return (NoOverwrite,[cmd],[],[])
    Encode f ts ds d ov → do output_dir ← pResolve d
                             f'         ← pResolve f
                             (cmds,fns,dirs) ← encodes f' output_dir ts ds
                             return (ov,cmds,fns,dirs)
  checkRunNICmds' overwrite cmds fns dirs do_mock
  return 0

----------------------------------------

main ∷ IO ()
main = do
  let progDesc = "HandBrakeCLI wrapper"
  wd ← ӝ (getCwd @FPathIOError)
  getArgs ≫ stdMain progDesc (parseOptions wd) (myMain @UsageFPProcIOError wd)

-- that's all, folks! ----------------------------------------------------------
