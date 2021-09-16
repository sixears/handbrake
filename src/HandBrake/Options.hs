module HandBrake.Options
  ( HasOptions, Options( Encode, Scan )
  , details, input, outputDir, overwrite, parseOptions, titles )
where

-- base --------------------------------

import Control.Applicative   ( many, optional )
import Data.List.NonEmpty    ( NonEmpty( (:|) ) )
import Data.Function         ( ($), id )
import System.IO             ( FilePath )
import Text.Show             ( Show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- fpath -------------------------------

import FPath.AbsDir      ( AbsDir )
import FPath.AsFilePath  ( filepath )
import FPath.File        ( File )
import FPath.Parseable   ( Parseable( readM ) )

-- lens --------------------------------

import Control.Lens.Lens  ( Lens', lens )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊵) )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Lens         ( (⫥) )
import Data.MoreUnicode.Maybe        ( 𝕄 )
import Data.MoreUnicode.Monoid       ( ю )
import Data.MoreUnicode.Text         ( 𝕋 )

-- natural -----------------------------

import Natural  ( ℕ )

-- optparse-applicative ----------------

import Options.Applicative.Builder  ( argument, auto, command, flag, help, info
                                    , metavar, long, progDesc, short
                                    , strOption, value )
import Options.Applicative.Extra    ( hsubparser )
import Options.Applicative.Types    ( Parser, ParserInfo )

-- stdmain -----------------------------

import StdMain  ( Overwrite( NoOverwrite, Overwrite ) )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import HandBrake.Encode  ( EncodeDetails, parseEncodeDetails, readNT )

--------------------------------------------------------------------------------

data Options = Scan File (𝕄 ℕ)
             | Encode { _input     ∷ File
                      , _titles    ∷ NonEmpty (ℕ,𝕋)
                      , _details   ∷ EncodeDetails
                      , _outputDir ∷ FilePath
                      , _overwrite ∷ Overwrite
                      }
  deriving Show

--------------------

class HasOptions α where
  _Options  ∷ Lens' α Options
  input     ∷ Lens' α File
  input     = _Options ∘ input
  titles    ∷ Lens' α (NonEmpty (ℕ,𝕋))
  titles    = _Options ∘ titles
  details   ∷ Lens' α EncodeDetails
  details   = _Options ∘ details
  outputDir ∷ Lens' α FilePath
  outputDir = _Options ∘ outputDir
  overwrite ∷ Lens' α Overwrite
  overwrite = _Options ∘ overwrite

--------------------

instance HasOptions Options where
  _Options = id
  input     = lens _input     (\ op i → op { _input     = i })
  titles    = lens _titles    (\ op t → op { _titles    = t })
  details   = lens _details   (\ op d → op { _details   = d })
  outputDir = lens _outputDir (\ op d → op { _outputDir = d })
  overwrite = lens _overwrite (\ op o → op { _overwrite = o })

----------------------------------------

parseScanArgs ∷ Parser Options
parseScanArgs = Scan ⊳ argument readM (metavar "VIDEO")
                     ⊵ optional (argument auto $ metavar "TITLE#")

--------------------

parseScan ∷ ParserInfo Options
parseScan = info parseScanArgs (progDesc "scan a video file")

----------------------------------------

parseEncode ∷ AbsDir → ParserInfo Options
parseEncode d =
  let
    parseEncodeArgs ∷ Parser Options
    parseEncodeArgs =
      let
        parseNT ∷ Parser (ℕ,𝕋)
        parseNT = argument readNT (metavar "#=NAME")

        parseNE ∷ Parser α → Parser (NonEmpty α)
        parseNE p = (:|) ⊳ p ⊵ many p
      in
        Encode ⊳ argument readM (metavar "HOSTS.dhall")
               ⊵ parseNE parseNT
               ⊵ parseEncodeDetails
               ⊵ (strOption (ю [ short 'd', long "output-dir"
                                  , help "output dir", value $ d ⫥ filepath]))
               ⊵ flag NoOverwrite Overwrite (ю [ short 'O', long "overwrite"
                                               , help "overwrite extant files"])
   in
    info parseEncodeArgs (progDesc "encode a video stream")

----------------------------------------

parseOptions ∷ AbsDir → Parser Options
parseOptions d =
  hsubparser (command "scan" parseScan ⊕ command "encode" (parseEncode d))

-- that's all, folks! ----------------------------------------------------------
