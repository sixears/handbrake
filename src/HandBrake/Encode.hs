module HandBrake.Encode
  ( AudioCopy(..), EncodeRequest, Numbering(..), Profile(..), TwoPass(..)
  , audioCopy, audios, chapters, encodeArgs, encodeRequest, input, inputOffset
  , name, numbering, profile, quality, subtitles, title, twoPass
  )
where

import Prelude  ( Float, (+), fromIntegral )

-- base --------------------------------

import Control.Monad       ( return )
import Data.Eq             ( Eq )
import Data.Function       ( ($) )
import Data.List.NonEmpty  ( NonEmpty )
import Data.Maybe          ( fromMaybe, maybe )
import Data.Ord            ( (<) )
import GHC.Generics        ( Generic )
import Text.Show           ( Show( show ) )

-- base-unicode-symbols ----------------

import Prelude.Unicode          ( ℤ )
import Numeric.Natural.Unicode  ( ℕ )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), toText )

-- deepseq -----------------------------

import Control.DeepSeq  ( NFData )

-- fpath -------------------------------

import FPath.File  ( File )

-- lens --------------------------------

import Control.Lens.Lens  ( Lens', lens )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor   ( (⊳) )
import Data.MoreUnicode.Lens      ( (⊣) )
import Data.MoreUnicode.Maybe     ( 𝕄, pattern 𝕵, pattern 𝕹 )
import Data.MoreUnicode.Monoid    ( ю )
import Data.MoreUnicode.Text      ( 𝕋 )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

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


data EncodeRequest = EncodeRequest { _input       ∷ File
                                   , _title       ∷ ℕ
                                   , _name        ∷ 𝕄 𝕋
                                   , _inputOffset ∷ ℤ
                                   , _numbering   ∷ Numbering
                                   , _chapters    ∷ 𝕄 [ℕ]
                                   , _twoPass     ∷ TwoPass
                                   , _profile     ∷ Profile
                                   , _audios      ∷ NonEmpty ℕ
                                   -- first, if any, is the default
                                   -- this is because --subtitle-default
                                   -- a HandBrakeCLI argument is actually an
                                   -- index into the --subtitle list argument
                                   , _subtitles   ∷ [ℕ]
                                   -- 20 is default, use 26 for 1080p
                                   , _quality     ∷ 𝕄 Float
                                   , _audioCopy   ∷ AudioCopy
                                   }

--------------------

input ∷ Lens' EncodeRequest File
input = lens _input (\ er f → er { _input = f })

--------------------

title ∷ Lens' EncodeRequest ℕ
title = lens _title (\ er t → er { _title = t })

--------------------

name ∷ Lens' EncodeRequest (𝕄 𝕋)
name = lens _name (\ er n → er { _name = n })

--------------------

{- Output basename, sanitized for safety. -}
nameSafe ∷ EncodeRequest → 𝕄 𝕋
nameSafe er = map go ⊳ er ⊣ name
              where go '/' = '-'
                    go ':' = '-'
                    go c   = c

--------------------

inputOffset ∷ Lens' EncodeRequest ℤ
inputOffset = lens _inputOffset (\ er x → er { _inputOffset = x })

--------------------

outputNum ∷ (AsUsageError ε, MonadError ε η) ⇒ EncodeRequest → 𝕄 ℕ → η ℕ
outputNum er n =
  let n' = fromMaybe (er ⊣ title) n
      on = fromIntegral n' + er ⊣ inputOffset
   in if on < 1
      then throwUsage $
             [fmtT|output number %d (%d«%d) < 0|] on n' (er ⊣ inputOffset)
      else return $ fromIntegral on

--------------------

numbering ∷ Lens' EncodeRequest Numbering
numbering = lens _numbering (\ er x → er { _numbering = x })

--------------------

chapters ∷ Lens' EncodeRequest (𝕄 [ℕ])
chapters = lens _chapters (\ er x → er { _chapters = x })

--------------------

twoPass ∷ Lens' EncodeRequest TwoPass
twoPass = lens _twoPass (\ er x → er { _twoPass = x })

--------------------

profile ∷ Lens' EncodeRequest Profile
profile = lens _profile (\ er x → er { _profile = x })

--------------------

audios ∷ Lens' EncodeRequest (NonEmpty ℕ)
audios = lens _audios (\ er x → er { _audios = x })

--------------------

subtitles ∷ Lens' EncodeRequest [ℕ]
subtitles = lens _subtitles (\ er x → er { _subtitles = x })

--------------------

quality ∷ Lens' EncodeRequest (𝕄 Float)
quality = lens _quality (\ er x → er { _quality = x })

--------------------

audioCopy ∷ Lens' EncodeRequest AudioCopy
audioCopy = lens _audioCopy (\ er x → er { _audioCopy = x })

----------------------------------------

encodeRequest ∷ File → ℕ → 𝕄 𝕋 → NonEmpty ℕ → [ℕ] → EncodeRequest
encodeRequest i t n as ss =
  EncodeRequest { _input       = i
                , _title       = t
                , _name        = n
                , _inputOffset = 0
                , _numbering   = Number
                , _chapters    = 𝕹
                , _twoPass     = TwoPass
                , _profile     = ProfileH265_2160P
                , _audios      = as
                , _subtitles   = ss
                , _quality     = 𝕹
                , _audioCopy   = AudioCopy
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
               , "--title" , pack (show $ er ⊣ title)
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
             , [ "--audio", [fmt|%L|] (show ⊳ er ⊣ audios) ]
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
