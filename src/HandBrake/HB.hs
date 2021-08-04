module HandBrake.HB
  ( main )
where

-- base --------------------------------

import Control.Applicative  ( pure )
import Control.Monad        ( foldM, return )
import Data.Function        ( ($), (&) )
import Data.List.NonEmpty   ( NonEmpty, toList )
import Data.Maybe           ( maybe )
import Data.Word            ( Word8 )
import GHC.Stack            ( HasCallStack )
import System.IO            ( IO )
import Text.Show            ( show )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≢) )
import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable, toText )

-- fpath -------------------------------

import FPath.AbsDir            ( AbsDir )
import FPath.Error.FPathError  ( AsFPathError )
import FPath.File              ( File )

-- log-plus ----------------------------

import Log  ( Log )

-- logging-effect ----------------------

import Control.Monad.Log  ( LoggingT, MonadLog )

-- mockio ------------------------------

import MockIO  ( DoMock( DoMock, NoMock ) )

-- mockio-log --------------------------

import MockIO.Log  ( MockIOClass )

-- mockio-plus -------------------------

import MockIO.Process            ( (!) )
import MockIO.Process.MLCmdSpec  ( mkMLCmdW' )

-- monaderror-io -----------------------

import MonadError.IO.Error  ( AsIOError )

-- monadio-plus ------------------------

import MonadIO                        ( MonadIO )
import MonadIO.Error.CreateProcError  ( AsCreateProcError )
import MonadIO.Error.ProcExitError    ( AsProcExitError )
import MonadIO.File                   ( devnull )
import MonadIO.FPath                  ( getCwd )
import MonadIO.Process.CmdSpec        ( cwd )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens   ( (⊢), (⊩) )
import Data.MoreUnicode.Maybe  ( 𝕄, pattern 𝕵 )
import Data.MoreUnicode.Monad  ( (≫) )
import Data.MoreUnicode.Text   ( 𝕋 )

-- mtl ---------------------------------

import Control.Monad.Except  ( ExceptT, MonadError )

-- natural -----------------------------

import Natural  ( ℕ, One, count )

-- stdmain -----------------------------

import StdMain             ( stdMain'' )
import StdMain.StdOptions  ( DryRunLevel )
import StdMain.UsageError  ( AsUsageError, UsageFPProcIOError )

-- text --------------------------------

import Data.Text  ( pack )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified HandBrake.Paths  as  Paths

import HandBrake.Encode   ( AudioTracks( AudioTracks ), EncodeDetails
                          , encodeRequest, details, encodeArgs )
import HandBrake.Options  ( Options( Encode, Scan ), parseOptions )

--------------------------------------------------------------------------------

scan ∷ ∀ ε μ .
       (MonadIO μ, MonadError ε μ, Printable ε,
        AsIOError ε, AsFPathError ε, AsCreateProcError ε, AsProcExitError ε,
        MonadLog (Log MockIOClass) μ) ⇒
       File → 𝕄 ℕ → DoMock → μ Word8
scan f n do_mock = do
  let args = [ "--scan"
             , "--input", toText f
             , "--title", maybe "0" (pack ∘ show) n
             ]
      cmd  = mkMLCmdW' Paths.handbrakeCLI args do_mock
  (_,()) ← devnull ≫ \null → null ! cmd
  return 0

----------------------------------------

encode ∷ ∀ ε μ .
       (MonadIO μ,
        AsUsageError ε, AsIOError ε, AsFPathError ε, AsCreateProcError ε,
        AsProcExitError ε,
        MonadError ε μ, Printable ε,
        MonadLog (Log MockIOClass) μ) ⇒
       AbsDir → File → ℕ → 𝕋 → EncodeDetails → DoMock → μ Word8
encode output_dir file stream_id name ds do_mock = do
  let req = encodeRequest file stream_id (𝕵 name) (AudioTracks $ pure 1) [] & details ⊢ ds
  args ← encodeArgs req
  let cmd  = (mkMLCmdW' Paths.handbrakeCLI args do_mock) & cwd ⊩ output_dir
  (_,()) ← devnull ≫ \null → null ! cmd
  return 0

----------------------------------------

{- | Perform one encode, iff input exit value (expected to be the exit of prior
     encodes) is not zero. -}
encode1 ∷ (MonadIO μ,
           AsUsageError ε, AsIOError ε, AsFPathError ε,
           AsCreateProcError ε, AsProcExitError ε,
           MonadError ε μ, Printable ε,
           MonadLog (Log MockIOClass) μ) ⇒
          AbsDir → File → EncodeDetails → DoMock → Word8 → (ℕ,𝕋) → μ Word8
encode1 wd input os do_mock x (n,t) =
  case x of
    0 → encode wd input n t os do_mock
    _ → return x

--------------------

{- | Encode multiple titles from a single input file. -}
encodes ∷ ∀ ε μ .
          (MonadIO μ, Printable ε, MonadError ε μ,
           AsUsageError ε, AsIOError ε, AsFPathError ε, AsCreateProcError ε,
           AsProcExitError ε,
           MonadLog (Log MockIOClass) μ) ⇒
          File → NonEmpty (ℕ,𝕋) → EncodeDetails → DoMock → μ Word8
encodes input ts os do_mock =
  getCwd ≫ \ wd → foldM (encode1 wd input os do_mock) 0 (toList ts)

----------------------------------------

myMain ∷ ∀ ε .
         (HasCallStack, Printable ε, AsUsageError ε,
          AsIOError ε, AsProcExitError ε, AsCreateProcError ε, AsFPathError ε) ⇒
         DryRunLevel One → Options
       → LoggingT (Log MockIOClass) (ExceptT ε IO) Word8
myMain dry_run opts = do
  let do_mock = if 0 ≢ count dry_run then DoMock else NoMock
  case opts of
    Scan   f n     → scan f n do_mock
    Encode f ts ds → encodes f ts ds do_mock

----------------------------------------

main ∷ IO ()
main = let progDesc = "HandBrakeCLI wrapper"
        in stdMain'' progDesc parseOptions (myMain @UsageFPProcIOError)

-- that's all, folks! ----------------------------------------------------------
