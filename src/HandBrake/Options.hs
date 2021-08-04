module HandBrake.Options
  ( Options( Encode, Scan ), parseOptions )
where

-- base --------------------------------

import Control.Applicative   ( many, optional, some )
import Data.List.NonEmpty    ( NonEmpty( (:|) ) )
import Data.Function         ( ($) )
import Text.Read             ( read )
import Text.Show             ( Show )

-- base-unicode-symbols ----------------

import Data.Monoid.Unicode  ( (⊕) )

-- fpath -------------------------------

import FPath.File       ( File )
import FPath.Parseable  ( Parseable( readM ) )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊵), (⋪) )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Maybe        ( 𝕄 )
import Data.MoreUnicode.Text         ( 𝕋 )

-- natural -----------------------------

import Natural  ( ℕ )

-- optparse-applicative ----------------

import Options.Applicative.Builder  ( argument, auto, command, info, metavar
                                    , progDesc )
import Options.Applicative.Extra    ( hsubparser )
import Options.Applicative.Types    ( Parser, ParserInfo )

-- optparse-plus -----------------------

import OptParsePlus  ( parsecReader )

-- parsec-plus -------------------------

import ParsecPlus  ( Parsecable( parser ) )

-- parsers -----------------------------

import Text.Parser.Char  ( char, digit, noneOf )

-- text --------------------------------

import Data.Text  ( pack )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import HandBrake.Encode  ( EncodeDetails, parseEncodeDetails )

--------------------------------------------------------------------------------

data Options = Scan File (𝕄 ℕ)
             | Encode { _input ∷ File
                      , titles ∷ NonEmpty (ℕ,𝕋)
                      , details ∷ EncodeDetails
                      }
  deriving Show

----------------------------------------

parseScanArgs ∷ Parser Options
parseScanArgs = Scan ⊳ argument readM (metavar "VIDEO")
                     ⊵ optional (argument auto $ metavar "TITLE#")

--------------------

parseScan ∷ ParserInfo Options
parseScan = info parseScanArgs (progDesc "scan a video file")

----------------------------------------

newtype NumberedTitle = NumberedTitle { unNumberedTitle ∷ (ℕ,𝕋) }

----------

instance Parsecable NumberedTitle where
  parser = NumberedTitle ⊳ go
    where go = (,) ⊳ (read ⊳ (some digit)) ⋪ char '=' ⊵ pack ⊳ some (noneOf [])

--------------------

parseEncode ∷ ParserInfo Options
parseEncode =
  let
    parseEncodeArgs ∷ Parser Options
    parseEncodeArgs =
      let
        parseNT ∷ Parser (ℕ,𝕋)
        parseNT = argument (unNumberedTitle ⊳ parsecReader) (metavar "#=NAME")

        parseNE ∷ Parser α → Parser (NonEmpty α)
        parseNE p = (:|) ⊳ p ⊵ many p
      in
        Encode ⊳ argument readM (metavar "HOSTS.dhall")
               ⊵ parseNE parseNT
               ⊵ parseEncodeDetails
   in
    info parseEncodeArgs (progDesc "encode a video stream")

----------------------------------------

parseOptions ∷ Parser Options
parseOptions =
  hsubparser ( command "scan"   parseScan ⊕ command "encode" parseEncode)

-- that's all, folks! ----------------------------------------------------------
