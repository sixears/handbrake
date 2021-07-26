module HandBrake.HB
  ( main )
where

-- base --------------------------------

import Control.Monad  ( return )
import Data.Maybe     ( maybe )
import Data.Word      ( Word8 )
import GHC.Stack      ( HasCallStack )
import System.IO      ( IO )
import Text.Show      ( show )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≢) )
import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable, toText )

-- fpath -------------------------------

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

-- more-unicode ------------------------

import Data.MoreUnicode.Maybe  ( 𝕄 )
import Data.MoreUnicode.Monad  ( (≫) )

-- mtl ---------------------------------

import Control.Monad.Except  ( ExceptT, MonadError )

-- natural -----------------------------

import Natural  ( ℕ, One, count )

-- stdmain -----------------------------

import StdMain             ( stdMain'' )
import StdMain.StdOptions  ( DryRunLevel )
import StdMain.UsageError  ( UsageFPProcIOError )

-- text --------------------------------

import Data.Text  ( pack )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified HandBrake.Paths  as  Paths

import HandBrake.Encode   ( EncodeRequest )
import HandBrake.Options  ( Options( Scan ), parseOptions )

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

-- encode
-- --input <FILENAME|DEVICE> --title <TITLE#>
-- outfn: NAME.MKV, prepend with %02d- (inputoffset+TITLE#)  unless nonumber
--          NAME must have /: replaced with --; HandBrakeCLI crashes if
--          outputting to a file with ':' in the name
--        If a Series is given, then %s - %02dx%02d - %s.mkv (remember /:)
--             or %s - %02dx%02d if no episode name
--        More title chaos for chapters
-- ? --chapters CHAPTERS
-- -2 -T if TwoPass (default?)
-- --markers --preset PROFILE --deinterlace --audio AUDIO,AUDIO,…
-- --subtitle SUB,SUB,… --subtitle-default DEFAULT_SUB
-- ? --quality FLOAT
-- -E copy if AudioCopy

----------------------------------------

myMain ∷ ∀ ε .
         (HasCallStack, Printable ε,
          AsIOError ε, AsProcExitError ε, AsCreateProcError ε, AsFPathError ε) ⇒
         DryRunLevel One → Options
       → LoggingT (Log MockIOClass) (ExceptT ε IO) Word8
myMain dry_run opts = do
  let do_mock = if 0 ≢ count dry_run then DoMock else NoMock
  case opts of
    Scan f n → scan f n do_mock

----------------------------------------

main ∷ IO ()
main = let progDesc = "HandBrakeCLI wrapper"
        in stdMain'' progDesc parseOptions (myMain @UsageFPProcIOError)

-- that's all, folks! ----------------------------------------------------------
