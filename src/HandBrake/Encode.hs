module HandBrake.Encode
  ( AudioTracks(..), Chapters(..), EncodeDetails, EncodeRequest, Numbering(..)
  , Profile(..), SubtitleTracks( SubtitleTracks ), TwoPass(..)

  , audioEncoder, audios, chapters, details, encodeArgs, encodeRequest
  , encodeRequest1, input, name, numbering, options, outputDir, outputName
  , profile, quality, subtitles, titleID, twoPass

  , parseEncodeDetails

  , readNT
  )
where

import Prelude  ( Float, (+), (-), fromIntegral )

-- base --------------------------------

import Control.Applicative  ( optional, pure, some )
import Control.Monad        ( mapM, return )
import Data.Eq              ( Eq )
import Data.Function        ( ($), id )
import Data.List.NonEmpty   ( NonEmpty( (:|) ) )
import Data.Maybe           ( maybe )
import Data.Ord             ( Ordering( GT, EQ, LT ), (<), compare )
import Data.String          ( unwords )
import GHC.Generics         ( Generic )
import Text.Read            ( read )
import Text.Show            ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode          ( (≡) )
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

import Data.MoreUnicode.Applicative  ( (⋪), (⋫), (⊵), (∤) )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Lens         ( (⊣) )
import Data.MoreUnicode.Maybe        ( 𝕄, pattern 𝕵, pattern 𝕹 )
import Data.MoreUnicode.Monad        ( (≫) )
import Data.MoreUnicode.Monoid       ( ю )
import Data.MoreUnicode.Text         ( 𝕋 )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- optparse-applicative ----------------

import Options.Applicative.Builder  ( auto, flag, flag', help
                                    , long, option, short, strOption, value )
import Options.Applicative.Types    ( Parser )

-- optparse-plus -----------------------

import OptParsePlus  ( parsecReader, parsecReadM, readNT )

-- parsec-plus -------------------------

import ParsecPlus  ( Parsecable( parser ) )

-- parsers -----------------------------

import Text.Parser.Char         ( CharParsing, char, digit, string )
import Text.Parser.Combinators  ( sepBy, eof )

-- parser-plus -------------------------

import ParserPlus  ( parseFloat2_1, sepByNE, tries )

-- range -------------------------------

import Data.Range  ( Bound( Bound ), BoundType( Exclusive, Inclusive ),
                     Range( InfiniteRange, LowerBoundRange, SingletonRange
                          , SpanRange, UpperBoundRange )
                   , (+=+)
                   )

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
               -- Dead Video is a super-simple video profile, meant for
               -- throwaway video (e.g., tracks where the video is static,
               -- that we'll later just rip the audio from)
               | Profile_DeadVideo
  deriving (Eq,Generic,NFData,Show)

instance Printable Profile where
  print ProfileH265_2160P = P.text "H.265 MKV 2160p60"
  print ProfileH265_1080P = P.text "H.265 MKV 1080p30"
  print ProfileH265_720P  = P.text "H.265 MKV 720p30"
  print ProfileH265_576P  = P.text "H.265 MKV 576p25"
  print ProfileH265_480P  = P.text "H.265 MKV 480p30"
  print Profile_DeadVideo = P.text "H.265 MKV 480p30"

instance Parsecable Profile where
  parser = let names ∷ CharParsing ψ ⇒ (NonEmpty (ψ Profile))
               names =  (pure ProfileH265_2160P ⋪ string "2160")
                    :| [ pure ProfileH265_1080P ⋪ string "1080"
                       , pure ProfileH265_2160P ⋪ string "1080"
                       , pure ProfileH265_720P  ⋪ string  "720"
                       , pure ProfileH265_576P  ⋪ string  "576"
                       , pure ProfileH265_480P  ⋪ string  "480"
                       , pure Profile_DeadVideo ⋪ string  "D"
                       ]
            in tries names ⋪ optional (char 'p' ∤ char 'P') ⋪ eof

parseProfile ∷ Parser Profile
parseProfile =
  option parsecReader (ю [ short 'p', long "profile" , help "encoding profile"
                         , value ProfileH265_2160P ])

------------------------------------------------------------

newtype Chapters = Chapters { unChapters ∷ 𝕄 (Range ℕ) }

instance Show Chapters where
  show c = [fmt|Chapters «%T»|] c

instance Printable Chapters where
  print (Chapters 𝕹)                       = P.text ""
  print (Chapters (𝕵 (SingletonRange n)))  = P.text $ [fmt|%d|] n
  print (Chapters (𝕵 (InfiniteRange)))     = P.text $ [fmt|-|]
  print (Chapters (𝕵 (LowerBoundRange (Bound a Inclusive)))) =
    P.text $ [fmt|[%d-|] a
  print (Chapters (𝕵 (LowerBoundRange (Bound a Exclusive)))) =
    P.text $ [fmt|(%d-|] a
  print (Chapters (𝕵 (UpperBoundRange (Bound a Inclusive)))) =
    P.text $ [fmt|-%d]|] a
  print (Chapters (𝕵 (UpperBoundRange (Bound a Exclusive)))) =
    P.text $ [fmt|-%d)|] a
  print (Chapters (𝕵 (SpanRange (Bound a Inclusive) (Bound b Inclusive)))) =
    P.text $ [fmt|[%d-%d]|] a b
  print (Chapters (𝕵 (SpanRange (Bound a Exclusive) (Bound b Inclusive)))) =
    P.text $ [fmt|(%d-%d]|] a b
  print (Chapters (𝕵 (SpanRange (Bound a Inclusive) (Bound b Exclusive)))) =
    P.text $ [fmt|[%d-%d)|] a b
  print (Chapters (𝕵 (SpanRange (Bound a Exclusive) (Bound b Exclusive)))) =
    P.text $ [fmt|(%d-%d)|] a b

parseSimpleNRange ∷ CharParsing γ ⇒ γ (Range ℕ)
parseSimpleNRange =
  let readN ∷ CharParsing γ ⇒ γ ℕ
      readN = read ⊳ some digit
      toRange ∷ ℕ → 𝕄 ℕ → Range ℕ
      toRange a 𝕹     = SingletonRange a
      toRange a (𝕵 b) = a +=+ b
   in toRange ⊳ readN ⊵ optional (char '-' ⋫ readN)

instance Parsecable Chapters where
  parser = Chapters ∘ 𝕵 ⊳ parseSimpleNRange

parseChapters ∷ Parser Chapters
parseChapters =
  option parsecReader (ю [ short 'c', long "chapters", value (Chapters 𝕹)
                         , help "select chapters to encode" ])

------------------------------------------------------------

defaultQuality ∷ Float
defaultQuality = 26

parseQuality ∷ Parser Float
parseQuality =
  option (parsecReadM "-q" parseFloat2_1)
         (ю [ short 'q', long "quality", value defaultQuality
            , help $ [fmt|encoding quality (default %3.1f)|] defaultQuality
            ])

----------------------------------------

parseOutputName ∷ Parser (𝕄 PathComponent)
parseOutputName =
  let mods = ю [ short 'o', long "output", help "output file base name" ]
   in optional (option readM mods)

----------------------------------------

parseAudioEncoder ∷ Parser (𝕄 𝕋)
parseAudioEncoder =
  let mods = ю [ short 'E', long "aencoder", long "audio-encoder"
               , help "set audio encoder(s) (see HandBrakeCLI -E)" ]
   in optional (strOption mods)

------------------------------------------------------------

{- | Options that have standard values, but may be adjusted for encodes. -}
data EncodeOptions = EncodeOptions { _numbering    ∷ Numbering
                                   , _chapters     ∷ Chapters
                                   , _twoPass      ∷ TwoPass
                                   , _profile      ∷ Profile
                                   -- 20 is default, use 26 for 1080p
                                   , _quality      ∷ Float
                                   , _outputName   ∷ 𝕄 PathComponent
                                   , _audioEncoder ∷ 𝕄 𝕋
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
  quality        ∷ Lens' α Float
  quality        = _EncodeOptions ∘ quality
  outputName     ∷ Lens' α (𝕄 PathComponent)
  outputName     = _EncodeOptions ∘ outputName
  audioEncoder   ∷ Lens' α (𝕄 𝕋)
  audioEncoder   = _EncodeOptions ∘ audioEncoder

instance HasEncodeOptions EncodeOptions where
  _EncodeOptions = id
  numbering      = lens _numbering    (\ eo x → eo { _numbering    = x })
  chapters       = lens _chapters     (\ eo x → eo { _chapters     = x })
  twoPass        = lens _twoPass      (\ eo x → eo { _twoPass      = x })
  profile        = lens _profile      (\ eo x → eo { _profile      = x })
  quality        = lens _quality      (\ eo x → eo { _quality      = x })
  audioEncoder   = lens _audioEncoder (\ eo x → eo { _audioEncoder = x })
  outputName     = lens _outputName   (\ eo n → eo { _outputName   = n })

----------------------------------------

parseEncodeOptions ∷ Parser EncodeOptions
parseEncodeOptions =
  EncodeOptions ⊳ parseNumbering
                ⊵ parseChapters
                ⊵ parseTwoPass
                ⊵ parseProfile
                ⊵ parseQuality
                ⊵ parseOutputName
                ⊵ parseAudioEncoder

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
                        { _numbering    = Number 0
                        , _chapters     = Chapters 𝕹
                        , _twoPass      = TwoPass
                        , _profile      = ProfileH265_2160P
                        , _quality      = defaultQuality
                        , _outputName   = 𝕹
                        , _audioEncoder = 𝕹
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
  cs     ← mapM formatBoundedNRange (unChapters $ er ⊣ chapters)
  let args = ю [ [ "--input" , toText $ er ⊣ input
                 , "--title" , pack (show $ er ⊣ titleID)
                 , "--markers" -- chapter markers
                 ]
               , if er ⊣ profile ≡ Profile_DeadVideo then []
                                                     else [ "--deinterlace" ]
               , [ "--audio-copy-mask"
                 , "aac,ac3,eac3,truehd,dts,dtshd,mp3,flac" ]
               , case er ⊣ twoPass of
                   TwoPass   → if er ⊣ profile ≡ Profile_DeadVideo
                               then []
                               else [ "--two-pass", "--turbo" ]
                   NoTwoPass → []
               , [ "--preset", toText $ er ⊣ profile ]
               , case er ⊣ audioEncoder of
                   𝕹   → [ "--aencoder", "copy" ]
                   𝕵 t → [ "--aencoder", t ]
               , [ "--audio", [fmt|%L|] (show ⊳ unAudioTracks (er ⊣ audios)) ]
               , case unSubtitleTracks (er ⊣ subtitles) of
                   [] → []
                   ss → [ "--subtitle", [fmt|%L|] (show ⊳ ss)
                          -- note that with HandBrakeCLI, subtitle-default is an
                          -- index into the list provided to --subtitle.  If
                          -- this doesn't work, maybe it's 1-based…
                        , "--subtitle-default", "0" ]
               , [ "--quality", [fmt|%03.1f|] (er ⊣ quality) ]
               , maybe [] (\ c → ["--chapters" , [fmt|%t|] c]) cs
               , [ "--output", toText output ]
               ]
  return (args,output)

{- | Take a range, which must be a single SingletonRange or a single SpanRange,
     and format that as `x` or `y-z`.  For a span range, the lower bound must be
     less than or equal to the upper bound; XXX
 -}
formatBoundedNRange ∷ (AsUsageError ε, MonadError ε μ) ⇒ Range ℕ → μ 𝕋
formatBoundedNRange InfiniteRange      = throwUsage $ [fmtT|illegal range «-»|]
formatBoundedNRange (SingletonRange n) = return $ [fmt|%d|] n
formatBoundedNRange (LowerBoundRange (Bound a Inclusive)) =
  throwUsage $ [fmtT|illegal range «[%d-»|] a
formatBoundedNRange (LowerBoundRange (Bound a Exclusive)) =
  throwUsage $ [fmtT|illegal range «(%d-»|] a
formatBoundedNRange (UpperBoundRange (Bound a Inclusive)) =
  throwUsage $ [fmtT|illegal range «-%d]»|] a
formatBoundedNRange (UpperBoundRange (Bound a Exclusive)) =
  throwUsage $ [fmtT|illegal range «-%d)»|] a
formatBoundedNRange (SpanRange (Bound a Inclusive) (Bound b Inclusive)) =
  case compare a b of
    LT → return $ [fmt|%d-%d|] a b
    EQ → return $ [fmt|%d|] a
    GT → throwUsage $ [fmtT|Range [%d-%d] is inverted|] a b
formatBoundedNRange (SpanRange (Bound a Exclusive) (Bound b Inclusive)) =
  case compare (a+1) b of
    LT → return $ [fmt|%d-%d|] (a+1) b
    EQ → return $ [fmt|%d|] b
    GT → throwUsage $ [fmtT|Range (%d-%d] is inverted|] a b
formatBoundedNRange (SpanRange (Bound a Inclusive) (Bound b Exclusive)) =
  if b ≡ 0
  then throwUsage $ [fmtT|Range [%d-%d) is illegal|] a b
  else case compare a (b-1) of
         LT → return $ [fmt|%d-%d|] a (b-1)
         EQ → return $ [fmt|%d|] a
         GT → throwUsage $ [fmtT|Range [%d-%d) is inverted|] a b
formatBoundedNRange (SpanRange (Bound a Exclusive) (Bound b Exclusive)) =
  if b ≡ 0
  then throwUsage $ [fmtT|Range [%d-%d) is illegal|] a b
  else case compare (a+1) (b-1) of
         LT → return $ [fmt|%d-%d|] (a+1) (b-1)
         EQ → return $ [fmt|%d|] (a+1)
         GT → throwUsage $ [fmtT|Range (%d-%d) is inverted|] a b

-- that's all, folks! ----------------------------------------------------------
