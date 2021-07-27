module HandBrake.Options
  ( Options( Encode, Scan ), parseOptions )
where

-- base --------------------------------

import Control.Applicative   ( optional )
import Data.Function         ( ($) )

-- base-unicode-symbols ----------------

import Data.Monoid.Unicode  ( (⊕) )

-- fpath -------------------------------

import FPath.File       ( File )
import FPath.Parseable  ( Parseable( readM ) )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊵) )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Maybe        ( 𝕄 )

-- natural -----------------------------

import Natural  ( ℕ )

-- optparse-applicative ----------------

import Options.Applicative.Builder  ( argument, auto, command, info, metavar
                                    , progDesc, subparser )
import Options.Applicative.Types    ( Parser, ParserInfo )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

--------------------------------------------------------------------------------

data Options = Scan File (𝕄 ℕ)
             | Encode { _input ∷ File }

-- input ∷ Lens' Options File
-- input = lens _input (\ o i → o { _input = i })

----------------------------------------

parseScanArgs ∷ Parser Options
parseScanArgs = Scan ⊳ argument readM (metavar "VIDEO")
                     ⊵ optional (argument auto $ metavar "TITLE#")

parseScan ∷ ParserInfo Options
parseScan = info parseScanArgs (progDesc "scan a video file")

parseOptions ∷ Parser Options
parseOptions =
  subparser
    ( command "scan" parseScan
    ⊕ command "encode" (info (Encode ⊳ argument readM (metavar "HOSTS.dhall")) (progDesc "e"))
           )

-- that's all, folks! ----------------------------------------------------------
