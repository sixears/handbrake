module HandBrake.Encode
  ( AudioCopy(..), AudioTracks(..), Chapters(..), EncodeDetails, EncodeRequest
  , Numbering(..), Profile(..), SubtitleTracks( SubtitleTracks ), TwoPass(..)

  , audioCopy, audios, chapters, details, encodeArgs, encodeRequest
  , encodeRequest1, input, name, numbering, options, outputDir, outputName
  , profile, quality, subtitles, titleID, twoPass

  , parseEncodeDetails

  , readNT
  )
where

import Prelude  ( Float, (+), fromIntegral )

-- base --------------------------------

import Control.Applicative  ( optional, pure, some )
import Control.Monad        ( return )
import Data.Eq              ( Eq )
import Data.Function        ( ($), id )
import Data.List.NonEmpty   ( NonEmpty( (:|) ) )
import Data.Maybe           ( maybe )
import Data.Ord             ( (<) )
import Data.String          ( unwords )
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

import qualified  FPath.Parseable

import FPath.AbsDir            ( AbsDir )
import FPath.AbsFile           ( AbsFile )
import FPath.AppendableFPath   ( (⫻) )
import FPath.Error.FPathError  ( AsFPathError )
import FPath.Parseable         ( readM )
import FPath.PathComponent     ( PathComponent )
import FPath.RelFile           ( RelFile )

-- lens --------------------------------

import Control.Lens.Lens  ( Lens', lens )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⋪), (⊵), (∤) )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Lens         ( (⊣) )
import Data.MoreUnicode.Maybe        ( 𝕄, pattern 𝕵, pattern 𝕹 )
import Data.MoreUnicode.Monad        ( (≫) )
import Data.MoreUnicode.Monoid       ( ю )
import Data.MoreUnicode.Text         ( 𝕋 )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- nonempty-containers -----------------

import Data.Set.NonEmpty  ( NESet, fromList, toAscList )

-- optparse-applicative ----------------

import Options.Applicative.Builder  ( auto, flag, flag', help
                                    , long, option, short, value )
import Options.Applicative.Types    ( Parser )

-- optparse-plus -----------------------

import OptParsePlus  ( parsecReader, parsecReadM, readNT, readMCommaSet )

-- parsec-plus -------------------------

import ParsecPlus  ( Parsecable( parser ) )

-- parsers -----------------------------

import Text.Parser.Char         ( CharParsing, char, digit, string )
import Text.Parser.Combinators  ( sepBy, eof )

-- parser-plus -------------------------

import ParserPlus  ( parseFloat2_1, sepByNE, tries )

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

parseTwoPass ∷ Parser TwoPass
parseTwoPass = flag TwoPass NoTwoPass
                    (ю [ short 'S'
                       , long "--single-pass"
                       , help "single-pass encoding (faster, worse" ])


------------------------------------------------------------

data AudioCopy = AudioCopy | NoAudioCopy
  deriving (Eq,Generic,NFData,Show)

------------------------------------------------------------

data Numbering = NoNumber
               | Number ℤ       {- with output offset -}
               | Series (ℕ,𝕋) ℤ {- title, series number, output offset -}
  deriving (Eq,Generic,NFData,Show)

parseNumbering ∷ Parser Numbering
parseNumbering =
  let
    parseNumber ∷ Parser (ℤ → Numbering)
    parseNumber = pure Number
    parseSeries ∷ Parser (ℤ → Numbering)
    parseSeries = Series ⊳ option readNT (ю [ short 'e', long "series"
                                            , help "series NUM=NAME" ])
    parseOffset ∷ Parser ℤ
    parseOffset = option auto (ю [ short 'i', long "input-offset", value 0
                                 , help "offset output numbers" ])
    parseNoNumber ∷ Parser Numbering
    parseNoNumber = flag' NoNumber (short 'N' ⊕ long "no-number")
  in
    (((parseNumber ∤ parseSeries) ⊵ parseOffset) ∤ parseNoNumber)

------------------------------------------------------------

data Profile   = ProfileH265_2160P | ProfileH265_1080P | ProfileH265_720P
               | ProfileH265_576P  | ProfileH265_480P
  deriving (Eq,Generic,NFData,Show)

instance Printable Profile where
  print ProfileH265_2160P = P.text "H.265 MKV 2160p60"
  print ProfileH265_1080P = P.text "H.265 MKV 1080p30"
  print ProfileH265_720P  = P.text "H.265 MKV 720p30"
  print ProfileH265_576P  = P.text "H.265 MKV 576p25"
  print ProfileH265_480P  = P.text "H.265 MKV 480p30"

instance Parsecable Profile where
  parser = let names ∷ CharParsing ψ ⇒ (NonEmpty (ψ Profile))
               names =  (pure ProfileH265_2160P ⋪ string "2160")
                    :| [ pure ProfileH265_1080P ⋪ string "1080"
                       , pure ProfileH265_2160P ⋪ string "1080"
                       , pure ProfileH265_720P  ⋪ string  "720"
                       , pure ProfileH265_576P  ⋪ string  "576"
                       , pure ProfileH265_480P  ⋪ string  "480"
                       ]
            in tries names ⋪ optional (char 'p' ∤ char 'P') ⋪ eof

parseProfile ∷ Parser Profile
parseProfile =
  option parsecReader (ю [ short 'p', long "profile" , help "encoding profile"
                         , value ProfileH265_2160P ])

------------------------------------------------------------

newtype Chapters = Chapters { unChapters ∷ 𝕄 (NESet ℕ) }

instance Show Chapters where
  show (Chapters 𝕹)        = "«»"
  show (Chapters (𝕵 (cs))) = [fmt|«%L»|] (show ⊳ toAscList cs)

instance Parsecable Chapters where
  parser = Chapters ∘ 𝕵 ∘ fromList ⊳ sepByNE (read ⊳ some digit) (char ',')

parseChapters ∷ Parser Chapters
parseChapters =
  let reader = readMCommaSet "-c|--chapters" (read ⊳ some digit)
      mods   = short 'c' ⊕ long "chapters" ⊕ help "select chapters to encode"
   in Chapters ⊳ (optional $ option reader mods)

------------------------------------------------------------

parseQuality ∷ Parser (𝕄 Float)
parseQuality = option (𝕵 ⊳ parsecReadM "-q" parseFloat2_1)
                      (ю [ short 'q', long "quality", value 𝕹
                         , help "encoding quality (default 20)" ])

----------------------------------------

parseAudioCopy ∷ Parser AudioCopy
parseAudioCopy = flag NoAudioCopy AudioCopy
                      (ю [ long "no-ac", long "no-audio-copy"
                         , help "disable audio copy (re-encode audio)" ])

----------------------------------------

parseOutputName ∷ Parser (𝕄 PathComponent)
parseOutputName =
  let mods = ю [ short 'o', long "output", help "output file base name" ]
   in optional (option readM mods)

------------------------------------------------------------

{- | Options that have standard values, but may be adjusted for encodes. -}
data EncodeOptions = EncodeOptions { _numbering   ∷ Numbering
                                   , _chapters    ∷ Chapters
                                   , _twoPass     ∷ TwoPass
                                   , _profile     ∷ Profile
                                   -- 20 is default, use 26 for 1080p
                                   , _quality     ∷ 𝕄 Float
                                   , _audioCopy   ∷ AudioCopy
                                   , _outputName  ∷ 𝕄 PathComponent
                                   }
  deriving Show

------------------------------------------------------------

class HasEncodeOptions α where
  _EncodeOptions ∷ Lens' α EncodeOptions
  numbering      ∷ Lens' α Numbering
  numbering      = _EncodeOptions ∘ numbering
  chapters       ∷ Lens' α Chapters
  chapters       = _EncodeOptions ∘ chapters
  twoPass        ∷ Lens' α TwoPass
  twoPass        = _EncodeOptions ∘ twoPass
  profile        ∷ Lens' α Profile
  profile        = _EncodeOptions ∘ profile
  quality        ∷ Lens' α (𝕄 Float)
  quality        = _EncodeOptions ∘ quality
  audioCopy      ∷ Lens' α AudioCopy
  audioCopy      = _EncodeOptions ∘ audioCopy
  outputName     ∷ Lens' α (𝕄 PathComponent)
  outputName     = _EncodeOptions ∘ outputName

instance HasEncodeOptions EncodeOptions where
  _EncodeOptions = id
  numbering      = lens _numbering   (\ eo x → eo { _numbering   = x })
  chapters       = lens _chapters    (\ eo x → eo { _chapters    = x })
  twoPass        = lens _twoPass     (\ eo x → eo { _twoPass     = x })
  profile        = lens _profile     (\ eo x → eo { _profile     = x })
  quality        = lens _quality     (\ eo x → eo { _quality     = x })
  audioCopy      = lens _audioCopy   (\ eo x → eo { _audioCopy   = x })
  outputName     = lens _outputName  (\ eo n → eo { _outputName  = n })

----------------------------------------

parseEncodeOptions ∷ Parser EncodeOptions
parseEncodeOptions =
  EncodeOptions ⊳ parseNumbering
                ⊵ parseChapters
                ⊵ parseTwoPass
                ⊵ parseProfile -- pure ProfileH265_2160P
                ⊵ parseQuality
                ⊵ parseAudioCopy
                ⊵ parseOutputName

------------------------------------------------------------

newtype AudioTracks = AudioTracks { unAudioTracks ∷ NonEmpty ℕ }
  deriving Show

instance Parsecable AudioTracks where
  parser = AudioTracks ⊳ sepByNE (read ⊳ some digit) (char ',')

parseAudioTracks ∷ Parser AudioTracks
parseAudioTracks =
  option parsecReader
         (ю [ short 'a', long "audios"
            , help (unwords [ "audio track ids; as reported by scan.  These will"
                            , "be a 1-based index." ])
            ]
         )

------------------------------------------------------------

newtype SubtitleTracks = SubtitleTracks { unSubtitleTracks ∷ [ℕ] }
  deriving Show

instance Parsecable SubtitleTracks where
  parser = SubtitleTracks ⊳ sepBy (read ⊳ some digit) (char ',')

parseSubtitleTracks ∷ Parser SubtitleTracks
parseSubtitleTracks =
  option parsecReader
         (ю [ help (unwords [ "subtitle track ids; as reported by scan.  These"
                            , "will be a 1-based index.  The first subtitle"
                            , "selected will be set as the default."
                            ]
                   )
            , value $ SubtitleTracks []
            , short 's'
            , long "subs"
            , long "subtitles"
            ])

------------------------------------------------------------

{- | Everything that must be specified for an encode, 'cept input, titleID &
     name. -}
data EncodeDetails = EncodeDetails { _audios      ∷ AudioTracks
                                   -- first, if any, is the default
                                   -- this is because --subtitle-default
                                   -- a HandBrakeCLI argument is actually an
                                   -- index into the --subtitle list argument
                                   , _subtitles   ∷ SubtitleTracks
                                   , _options     ∷ EncodeOptions
                                   }
  deriving Show

parseEncodeDetails ∷ Parser EncodeDetails
parseEncodeDetails =
  EncodeDetails ⊳ parseAudioTracks
                ⊵ parseSubtitleTracks
                ⊵ parseEncodeOptions

class HasEncodeDetails α where
  _EncodeDetails ∷ Lens' α EncodeDetails
  audios         ∷ Lens' α AudioTracks
  audios         = _EncodeDetails ∘ audios
  subtitles      ∷ Lens' α SubtitleTracks
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

data EncodeRequest = EncodeRequest { _input      ∷ AbsFile
                                   , _titleID    ∷ ℕ
                                   , _name       ∷ 𝕄 𝕋
                                   , _details    ∷ EncodeDetails
                                   , _outputDir  ∷ AbsDir
                                   }
  deriving Show

class HasEncodeRequest α where
  _EncodeRequest ∷ Lens' α EncodeRequest
  input          ∷ Lens' α AbsFile
  input          = _EncodeRequest ∘ input
  titleID        ∷ Lens' α ℕ
  titleID        = _EncodeRequest ∘ titleID
  name           ∷ Lens' α (𝕄 𝕋)
  name           = _EncodeRequest ∘ name
  details        ∷ Lens' α EncodeDetails
  details        = _EncodeRequest ∘ details
  outputDir      ∷ Lens' α AbsDir
  outputDir      = _EncodeRequest ∘ outputDir

instance HasEncodeRequest EncodeRequest where
  _EncodeRequest = id
  titleID        = lens _titleID    (\ er t → er { _titleID    = t })
  name           = lens _name       (\ er n → er { _name       = n })
  input          = lens _input      (\ er f → er { _input      = f })
  details        = lens _details    (\ er d → er { _details    = d })
  outputDir      = lens _outputDir  (\ er d → er { _outputDir  = d })

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

outputNum ∷ (AsUsageError ε, MonadError ε η) ⇒ EncodeRequest → η ℕ
outputNum er =
  let (on,oo) = case er ⊣ numbering of
                  NoNumber    → (0,0)
                  Number o   → (o + (fromIntegral $ er ⊣ titleID),o)
                  Series _ o → (o + (fromIntegral $ er ⊣ titleID),o)
   in if on < 1
      then throwUsage $
             [fmtT|output number %d (%d+(%d)) < 0|] on (er ⊣ titleID) oo
      else return $ fromIntegral on

----------------------------------------

{- | Create a basic `EncodeRequest` with mandatory arguments. -}
encodeRequest ∷ AbsFile     -- ^ video input
              → AbsDir      -- ^ output directory
              → ℕ           -- ^ titleID within input to encode
              → 𝕄 𝕋        -- ^ output base name
              → AudioTracks -- ^ input audio IDs to encode
              → EncodeRequest
encodeRequest i d t n as =
  EncodeRequest { _input       = i
                , _titleID     = t
                , _name        = n
                , _outputDir   = d
                , _details     =
                  EncodeDetails
                    { _audios    = as
                    , _subtitles = SubtitleTracks $ []
                    , _options   =
                      EncodeOptions
                        { _numbering   = Number 0
                        , _chapters    = Chapters 𝕹
                        , _twoPass     = TwoPass
                        , _profile     = ProfileH265_2160P
                        , _quality     = 𝕹
                        , _audioCopy   = AudioCopy
                        , _outputName  = 𝕹
                        }
                    }
                }

{- | `encodeRequest` with single audio track 1. -}
encodeRequest1 ∷ AbsFile     -- ^ video input
               → AbsDir      -- ^ output dir
               → ℕ           -- ^ titleID within input to encode
               → 𝕄 𝕋        -- ^ output base name
               → EncodeRequest
encodeRequest1 i d t n = encodeRequest i d t n (AudioTracks $ pure 1)

----------------------------------------

{- | Implied output file of an `EncodeRequest`. -}
erImpliedName ∷ (AsUsageError ε, AsFPathError ε, MonadError ε η) ⇒
                EncodeRequest → η 𝕋
erImpliedName er = do
  case er ⊣ numbering of
        NoNumber        → case nameSafe er of
                            𝕵 n → return $ [fmtT|%t.mkv|] n
                            𝕹   → throwUsage $ [fmtT|no number & no title|]
        Number _        → do output_num ← outputNum er
                             case nameSafe er of
                               𝕵 n → return $ [fmtT|%02d-%t.mkv|] output_num n
                               𝕹   → return $ [fmtT|%02d.mkv|]    output_num
        Series (s,nm) _ → do output_num ← outputNum er
                             case nameSafe er of
                               𝕵 n → return $ [fmtT|%t - %02dx%02d - %t.mkv|]
                                                    nm   s output_num n
                               𝕹   → return $ [fmtT|%t - %02dx%02d.mkv|]
                                                    nm   s output_num

--------------------

{- | Chosen output file of an `EncodeRequest`. -}
erOutput ∷ (AsUsageError ε, AsFPathError ε, MonadError ε η) ⇒
           EncodeRequest → η AbsFile
erOutput er = do
  p ← case er ⊣ outputName of
        𝕹 → erImpliedName er ≫ FPath.Parseable.parse @RelFile
        𝕵 f → FPath.Parseable.parse @RelFile f
  return $ (er ⊣ outputDir) ⫻ p

--------------------

{- | Arguments to HandBrakeCLI for a given `EncodeRequest`. -}
encodeArgs ∷ ∀ ε η . (AsUsageError ε, AsFPathError ε, MonadError ε η) ⇒
             EncodeRequest → η ([𝕋],AbsFile)
encodeArgs er = do
  output ← erOutput er
  let args = ю [ [ "--input" , toText $ er ⊣ input
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
               , case unSubtitleTracks (er ⊣ subtitles) of
                   [] → []
                   ss → [ "--subtitle", [fmt|%L|] (show ⊳ ss)
                          -- note that with HandBrakeCLI, subtitle-default is an
                          -- index into the list provided to --subtitle.  If
                          -- this doesn't work, maybe it's 1-based…
                        , "--subtitle-default", "0" ]
               , case er ⊣ quality of
                   𝕵 q → [ "--quality", [fmt|%2.1f|] q ]
                   𝕹   → []
               , maybe [] (\ c → ["--chapters", [fmt|%L|] (show ⊳ toAscList c)])
                          (unChapters $ er ⊣ chapters)
               , [ "--output", toText output ]
               ]
  return (args,output)

-- that's all, folks! ----------------------------------------------------------
