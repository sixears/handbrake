module HandBrake.Encode
  ( AudioCopy(..), AudioTracks(..), EncodeDetails, EncodeRequest
  , Numbering(..), Profile(..), TwoPass(..)

  , audioCopy, audios, chapters, details, encodeArgs, encodeRequest, input
  , inputOffset, name, numbering, options, profile, quality, subtitles, titleID
  , twoPass

  , parseEncodeDetails
  )
where

import Prelude  ( Float, (+), fromIntegral )

-- base --------------------------------

import Control.Applicative  ( pure, some )
import Control.Monad        ( return )
import Data.Eq              ( Eq )
import Data.Function        ( ($), id )
import Data.List.NonEmpty   ( NonEmpty )
import Data.Maybe           ( fromMaybe, maybe )
import Data.Ord             ( (<) )
import GHC.Generics         ( Generic )
import Text.Read            ( read )
import Text.Show            ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Function.Unicode    ( (∘) )
import Data.Monoid.Unicode      ( (⊕) )
import Numeric.Natural.Unicode  ( ℕ )
import Prelude.Unicode          ( ℤ )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), toText )

-- deepseq -----------------------------

import Control.DeepSeq  ( NFData )

-- fpath -------------------------------

import FPath.File  ( File )

-- lens --------------------------------

import Control.Lens.Lens  ( Lens', lens )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊵) )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Lens         ( (⊣) )
import Data.MoreUnicode.Maybe        ( 𝕄, pattern 𝕵, pattern 𝕹 )
import Data.MoreUnicode.Monoid       ( ю )
import Data.MoreUnicode.Text         ( 𝕋 )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- optparse-applicative ----------------

import Options.Applicative.Builder  ( flag, help, long, option, short )
import Options.Applicative.Types    ( Parser )

-- optparse-plus -----------------------

import OptParsePlus  ( parsecReader, sepByNE )

-- parsec-plus -------------------------

import ParsecPlus  ( Parsecable( parser ) )

-- parsers -----------------------------

import Text.Parser.Char         ( char, digit )

-- stdmain -----------------------------

import StdMain.UsageError  ( AsUsageError, throwUsage )

-- text --------------------------------

import Data.Text  ( map, pack )

-- text-printer ------------------------

import qualified Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmt, fmtT )

--------------------------------------------------------------------------------

data TwoPass   = TwoPass   | NoTwoPass
  deriving (Eq,Generic,NFData,Show)
data AudioCopy = AudioCopy | NoAudioCopy
  deriving (Eq,Generic,NFData,Show)
data Numbering = NoNumber | Number | Series 𝕋 ℕ
  deriving (Eq,Generic,NFData,Show)
data Profile   = ProfileH265_2160P | ProfileH265_1080P | ProfileH265_720P
               | ProfileH265_576P  | ProfileH265_480P
  deriving (Eq,Generic,NFData,Show)

instance Printable Profile where
  print ProfileH265_2160P = P.text "H.265 MKV 2160p60"
  print ProfileH265_1080P = P.text "H.265 MKV 1080p30"
  print ProfileH265_720P  = P.text "H.265 MKV 720p30"
  print ProfileH265_576P  = P.text "H.265 MKV 576p25"
  print ProfileH265_480P  = P.text "H.265 MKV 480p30"

------------------------------------------------------------

{- | Options that have standard values, but may be adjusted for encodes. -}
data EncodeOptions = EncodeOptions { _inputOffset ∷ ℤ
                                   , _numbering   ∷ Numbering
                                   , _chapters    ∷ 𝕄 [ℕ]
                                   , _twoPass     ∷ TwoPass
                                   , _profile     ∷ Profile
                                   -- 20 is default, use 26 for 1080p
                                   , _quality     ∷ 𝕄 Float
                                   , _audioCopy   ∷ AudioCopy
                                   }
  deriving Show

parseEncodeOptions ∷ Parser EncodeOptions
parseEncodeOptions =
  EncodeOptions ⊳ pure 0
                ⊵ pure Number
                ⊵ pure 𝕹
                ⊵ pure TwoPass
                ⊵ pure ProfileH265_2160P
                ⊵ pure 𝕹
                ⊵ flag NoAudioCopy AudioCopy
                       (ю [ long "no-ac", long "no-audio-copy"
                          , help "disable audio copy (re-encode audio)" ])

class HasEncodeOptions α where
  _EncodeOptions ∷ Lens' α EncodeOptions
  inputOffset    ∷ Lens' α ℤ
  inputOffset    = _EncodeOptions ∘ inputOffset
  numbering      ∷ Lens' α Numbering
  numbering      = _EncodeOptions ∘ numbering
  chapters       ∷ Lens' α (𝕄 [ℕ])
  chapters       = _EncodeOptions ∘ chapters
  twoPass        ∷ Lens' α TwoPass
  twoPass        = _EncodeOptions ∘ twoPass
  profile        ∷ Lens' α Profile
  profile        = _EncodeOptions ∘ profile
  quality        ∷ Lens' α (𝕄 Float)
  quality        = _EncodeOptions ∘ quality
  audioCopy      ∷ Lens' α AudioCopy
  audioCopy      = _EncodeOptions ∘ audioCopy

instance HasEncodeOptions EncodeOptions where
  _EncodeOptions = id
  inputOffset    = lens _inputOffset (\ eo x → eo { _inputOffset = x })
  numbering      = lens _numbering   (\ eo x → eo { _numbering   = x })
  chapters       = lens _chapters    (\ eo x → eo { _chapters    = x })
  twoPass        = lens _twoPass     (\ eo x → eo { _twoPass     = x })
  profile        = lens _profile     (\ eo x → eo { _profile     = x })
  quality        = lens _quality     (\ eo x → eo { _quality     = x })
  audioCopy      = lens _audioCopy   (\ eo x → eo { _audioCopy   = x })

------------------------------------------------------------

newtype AudioTracks = AudioTracks { unAudioTracks ∷ NonEmpty ℕ }
  deriving Show

instance Parsecable AudioTracks where
  parser = AudioTracks ⊳ sepByNE (read ⊳ some digit) (char ',')

parseAudioTracks ∷ Parser AudioTracks
parseAudioTracks =
  option parsecReader (help "audio track ids" ⊕ short 'a' ⊕ long "audio")

{- | Everything that must be specified for an encode, 'cept input, titleID &
     name. -}
data EncodeDetails = EncodeDetails { _audios      ∷ AudioTracks
                                   -- first, if any, is the default
                                   -- this is because --subtitle-default
                                   -- a HandBrakeCLI argument is actually an
                                   -- index into the --subtitle list argument
                                   , _subtitles   ∷ [ℕ]
                                   , _options     ∷ EncodeOptions
                                   }
  deriving Show

parseEncodeDetails ∷ Parser EncodeDetails
parseEncodeDetails =
  EncodeDetails ⊳ parseAudioTracks
                ⊵ pure []
                ⊵ parseEncodeOptions

class HasEncodeDetails α where
  _EncodeDetails ∷ Lens' α EncodeDetails
  audios         ∷ Lens' α AudioTracks
  audios         = _EncodeDetails ∘ audios
  subtitles      ∷ Lens' α [ℕ]
  subtitles      = _EncodeDetails ∘ subtitles
  options        ∷ Lens' α EncodeOptions
  options        = _EncodeDetails ∘ options

instance HasEncodeDetails EncodeDetails where
  _EncodeDetails = id
  audios         = lens _audios    (\ ed x → ed { _audios    = x })
  subtitles      = lens _subtitles (\ ed x → ed { _subtitles = x })
  options        = lens _options   (\ ed x → ed { _options   = x })

instance HasEncodeOptions EncodeDetails where
  _EncodeOptions = options

------------------------------------------------------------

data EncodeRequest = EncodeRequest { _input       ∷ File
                                   , _titleID     ∷ ℕ
                                   , _name        ∷ 𝕄 𝕋
                                   , _details     ∷ EncodeDetails
                                   }
  deriving Show

class HasEncodeRequest α where
  _EncodeRequest ∷ Lens' α EncodeRequest
  input          ∷ Lens' α File
  input          = _EncodeRequest ∘ input
  titleID        ∷ Lens' α ℕ
  titleID        = _EncodeRequest ∘ titleID
  name           ∷ Lens' α (𝕄 𝕋)
  name           = _EncodeRequest ∘ name
  details        ∷ Lens' α EncodeDetails
  details        = _EncodeRequest ∘ details

instance HasEncodeRequest EncodeRequest where
  _EncodeRequest = id
  titleID        = lens _titleID (\ er t → er { _titleID = t })
  name           = lens _name    (\ er n → er { _name    = n })
  input          = lens _input   (\ er f → er { _input   = f })
  details        = lens _details (\ ed x → ed { _details   = x })

instance HasEncodeDetails EncodeRequest where
  _EncodeDetails = details

instance HasEncodeOptions EncodeRequest where
  _EncodeOptions = details ∘ options

------------------------------------------------------------

{- Output basename, sanitized for safety. -}
nameSafe ∷ EncodeRequest → 𝕄 𝕋
nameSafe er = map go ⊳ er ⊣ name
              where go '/' = '-'
                    go ':' = '-'
                    go c   = c

--------------------

outputNum ∷ (AsUsageError ε, MonadError ε η) ⇒ EncodeRequest → 𝕄 ℕ → η ℕ
outputNum er n =
  let n' = fromMaybe (er ⊣ titleID) n
      on = fromIntegral n' + er ⊣ inputOffset
   in if on < 1
      then throwUsage $
             [fmtT|output number %d (%d«%d) < 0|] on n' (er ⊣ inputOffset)
      else return $ fromIntegral on

----------------------------------------

{- | Create a basic `EncodeRequest` with mandatory arguments. -}
encodeRequest ∷ File        -- ^ video input
              → ℕ           -- ^ titleID within input to encode
              → 𝕄 𝕋        -- ^ output base name
              → AudioTracks -- ^ input audio IDs to encode
              → [ℕ]         -- ^ input subtitle IDs to encode (first will be
                            --   marked as default
              → EncodeRequest
encodeRequest i t n as ss =
  EncodeRequest { _input       = i
                , _titleID     = t
                , _name        = n
                , _details     =
                  EncodeDetails
                    { _audios    = as
                    , _subtitles = ss
                    , _options   =
                      EncodeOptions
                        { _inputOffset = 0
                        , _numbering   = Number
                        , _chapters    = 𝕹
                        , _twoPass     = TwoPass
                        , _profile     = ProfileH265_2160P
                        , _quality     = 𝕹
                        , _audioCopy   = AudioCopy
                        }
                    }
                }

----------------------------------------

encodeArgs ∷ (AsUsageError ε, MonadError ε η) ⇒ EncodeRequest → η [𝕋]
encodeArgs er = do
  output ← case er ⊣ numbering of
             NoNumber     → case nameSafe er of
                              𝕵 n → return $ [fmtT|%t.mkv|] n
                              𝕹   → throwUsage $ [fmtT|no number & no title|]
             Number       → do output_num ← outputNum er 𝕹
                               case nameSafe er of
                                 𝕵 n → return $ [fmtT|%02d-%t.mkv|] output_num n
                                 𝕹   → return $ [fmtT|%02d.mkv|]    output_num
             Series nm ss → do output_num ← outputNum er 𝕹
                               case nameSafe er of
                                 𝕵 n → return $ [fmtT|%t - %02dx%02d - %t.mkv|]
                                                      nm   ss    output_num n
                                 𝕹   → return $ [fmtT|%t - %02dx%02d.mkv|]
                                                      nm   ss    output_num

  return $ ю [ [ "--input" , toText $ er ⊣ input
               , "--title" , pack (show $ er ⊣ titleID)
               , "--markers" -- chapter markers
               , "--deinterlace"
               ]
             , case er ⊣ twoPass of
                 TwoPass   → [ "--two-pass", "--turbo" ]
                 NoTwoPass → []
             , [ "--preset", toText $ er ⊣ profile ]
             , case er ⊣ audioCopy of
                 AudioCopy   → [ "--aencoder", "copy" ]
                 NoAudioCopy → []
             , [ "--audio", [fmt|%L|] (show ⊳ unAudioTracks (er ⊣ audios)) ]
             , case er ⊣ subtitles of
                 [] → []
                 ss → [ "--subtitle", [fmt|%L|] (show ⊳ ss)
                        -- note that with HandBrakeCLI, subtitle-default is an
                        -- index into the list provided to --subtitle.  If this
                        -- doesn't work, maybe it's 1-based…
                      , "--subtitle-default=0" ]
             , case er ⊣ quality of
                 𝕵 q → [ "--quality", [fmt|%2.1f|] q ]
                 𝕹   → []
             , maybe [] (\ cs → ["--chapters", [fmt|%L|] (show ⊳ cs)])
                        (er ⊣ chapters)
             , [ "--output", toText output ]
             ]

-- that's all, folks! ----------------------------------------------------------
