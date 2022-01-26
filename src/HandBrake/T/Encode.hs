module HandBrake.T.Encode
  ( tests )
where

-- base --------------------------------

import Control.Applicative  ( pure )
import Data.Function        ( ($), (&) )
import Data.List.NonEmpty   ( NonEmpty( (:|) ) )
import Data.Tuple           ( fst )
import System.Exit          ( ExitCode )
import System.IO            ( IO )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- fpath -------------------------------

import FPath.AbsDir         ( absdir, root )
import FPath.AbsFile        ( absfile )
import FPath.PathComponent  ( pc )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor  ( (⊳) )
import Data.MoreUnicode.Lens     ( (⊢), (⊩) )
import Data.MoreUnicode.Maybe    ( pattern 𝕵, pattern 𝕹 )
import Data.MoreUnicode.Natural  ( ℕ )
import Data.MoreUnicode.String   ( 𝕊 )
import Data.MoreUnicode.Text     ( 𝕋 )

-- range -------------------------------

import Data.Range  ( Bound( Bound ), BoundType( Inclusive )
                   , Range( LowerBoundRange, SingletonRange ), (+=+) )

-- stdmain -----------------------------

import StdMain.UsageError  ( UsageFPathIOError, usageError )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( testCase )

-- tasty-plus --------------------------

import TastyPlus  ( (≟), assertLeft, assertListEqR
                  , runTestsP, runTestsReplay, runTestTree )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import HandBrake.Encode  ( AudioTracks( AudioTracks )
                         , Chapters( Chapters )
                         , Numbering( NoNumber, Number, Series )
                         , Profile( ProfileH265_576P, ProfileH265_720P
                                  , Profile_DeadVideo )
                         , SubtitleTracks( SubtitleTracks )
                         , TwoPass( NoTwoPass )
                         , audios, audioEncoder, chapters, encodeArgs
                         , encodeRequest, input, name, numbering, outputDir
                         , outputName, profile, quality, subtitles, titleID
                         , twoPass
                         )

--------------------------------------------------------------------------------

tests ∷ TestTree
tests =
  testGroup "Encode" $
    let
      testEncode nm req exp =
        testGroup nm $
          assertListEqR nm (fst ⊳ encodeArgs @UsageFPathIOError req) exp
      base_req = encodeRequest [absfile|/nonesuch|] root 3 (𝕵 "bob")
                               (AudioTracks $ pure 2)
                   & subtitles ⊢ (SubtitleTracks [3,4])
      usage_error nm txt req =
        testCase nm $
          assertLeft (usageError @𝕋 @UsageFPathIOError txt ≟) (encodeArgs req)
     in
    [ testEncode "base_req" base_req
                 [ "--input"   , "/nonesuch"
                 , "--title"   , "3"
                 , "--markers"
                 , "--deinterlace"
                 , "--audio-copy-mask", "aac,ac3,eac3,truehd,dts,dtshd,mp3,flac"
                 , "--two-pass", "--turbo"
                 , "--preset", "H.265 MKV 2160p60"
                 , "--aencoder", "copy"
                 , "--audio"   , "2"
                 , "--subtitle", "3,4", "--subtitle-default", "0"
                 , "--quality" , "26.0"
                 , "--output"  , "/03-bob.mkv"
                 ]
    , testEncode "inputOffset 2" (base_req & numbering ⊢ Number 2)
                 [ "--input"   , "/nonesuch"
                 , "--title"   , "3"
                 , "--markers"
                 , "--deinterlace"
                 , "--audio-copy-mask", "aac,ac3,eac3,truehd,dts,dtshd,mp3,flac"
                 , "--two-pass", "--turbo"
                 , "--preset", "H.265 MKV 2160p60"
                 , "--aencoder","copy"
                 , "--audio"   , "2"
                 , "--subtitle", "3,4", "--subtitle-default", "0"
                 , "--quality" , "26.0"
                 , "--output"  , "/05-bob.mkv"
                 ]
    , usage_error "inputOffset -3"
                  "output number 0 (3+(-3)) < 0"
                  (base_req & numbering ⊢ Number (-3))
    , testEncode "NoNumber" (base_req & numbering ⊢ NoNumber)
                 [ "--input"   , "/nonesuch"
                 , "--title"   , "3"
                 , "--markers"
                 , "--deinterlace"
                 , "--audio-copy-mask", "aac,ac3,eac3,truehd,dts,dtshd,mp3,flac"
                 , "--two-pass", "--turbo"
                 , "--preset", "H.265 MKV 2160p60"
                 , "--aencoder","copy"
                 , "--audio"   , "2"
                 , "--subtitle", "3,4", "--subtitle-default", "0"
                 , "--quality" , "26.0"
                 , "--output"  , "/bob.mkv"
                 ]
    , testEncode "Series S 5" (base_req & numbering ⊢ Series (5,"S") 0)
                 [ "--input"   , "/nonesuch"
                 , "--title"   , "3"
                 , "--markers"
                 , "--deinterlace"
                 , "--audio-copy-mask", "aac,ac3,eac3,truehd,dts,dtshd,mp3,flac"
                 , "--two-pass", "--turbo"
                 , "--preset", "H.265 MKV 2160p60"
                 , "--aencoder","copy"
                 , "--audio"   , "2"
                 , "--subtitle", "3,4", "--subtitle-default", "0"
                 , "--quality" , "26.0"
                 , "--output"  , "/S - 05x03 - bob.mkv"
                 ]
    , testEncode "Series S 6, no name"
                 (base_req & numbering ⊢ Series (6,"S") 1 & name ⊢ 𝕹)
                 [ "--input"   , "/nonesuch"
                 , "--title"   , "3"
                 , "--markers"
                 , "--deinterlace"
                 , "--audio-copy-mask", "aac,ac3,eac3,truehd,dts,dtshd,mp3,flac"
                 , "--two-pass", "--turbo"
                 , "--preset", "H.265 MKV 2160p60"
                 , "--aencoder","copy"
                 , "--audio"   , "2"
                 , "--subtitle", "3,4", "--subtitle-default", "0"
                 , "--quality" , "26.0"
                 , "--output"  , "/S - 06x04.mkv"
                 ]
    , testEncode "chapters 6,7" (base_req & chapters
                                          ⊢ (Chapters $ 𝕵 (6 +=+ 7)))
                 [ "--input"   , "/nonesuch"
                 , "--title"   , "3"
                 , "--markers"
                 , "--deinterlace"
                 , "--audio-copy-mask", "aac,ac3,eac3,truehd,dts,dtshd,mp3,flac"
                 , "--two-pass", "--turbo"
                 , "--preset", "H.265 MKV 2160p60"
                 , "--aencoder","copy"
                 , "--audio"   , "2"
                 , "--subtitle", "3,4", "--subtitle-default", "0"
                 , "--quality" , "26.0"
                 , "--chapters", "6-7"
                 , "--output"  , "/03-bob.mkv"
                 ]
    , testEncode "chapters 5"
                 (base_req & chapters ⊢ (Chapters ∘ 𝕵 $ SingletonRange 5))
                 [ "--input"   , "/nonesuch"
                 , "--title"   , "3"
                 , "--markers"
                 , "--deinterlace"
                 , "--audio-copy-mask", "aac,ac3,eac3,truehd,dts,dtshd,mp3,flac"
                 , "--two-pass", "--turbo"
                 , "--preset", "H.265 MKV 2160p60"
                 , "--aencoder","copy"
                 , "--audio"   , "2"
                 , "--subtitle", "3,4", "--subtitle-default", "0"
                 , "--quality" , "26.0"
                 , "--chapters", "5"
                 , "--output"  , "/03-bob.mkv"
                 ]
    , testEncode "no two pass" (base_req & twoPass ⊢ NoTwoPass)
                 [ "--input"   , "/nonesuch"
                 , "--title"   , "3"
                 , "--markers"
                 , "--deinterlace"
                 , "--audio-copy-mask", "aac,ac3,eac3,truehd,dts,dtshd,mp3,flac"
                 , "--preset", "H.265 MKV 2160p60"
                 , "--aencoder","copy"
                 , "--audio"   , "2"
                 , "--subtitle", "3,4", "--subtitle-default", "0"
                 , "--quality" , "26.0"
                 , "--output"  , "/03-bob.mkv"
                 ]
    , testEncode "profile 576" (base_req & profile ⊢ ProfileH265_576P)
                 [ "--input"   , "/nonesuch"
                 , "--title"   , "3"
                 , "--markers"
                 , "--deinterlace"
                 , "--audio-copy-mask", "aac,ac3,eac3,truehd,dts,dtshd,mp3,flac"
                 , "--two-pass", "--turbo"
                 , "--preset", "H.265 MKV 576p25"
                 , "--aencoder","copy"
                 , "--audio"   , "2"
                 , "--subtitle", "3,4", "--subtitle-default", "0"
                 , "--quality" , "26.0"
                 , "--output"  , "/03-bob.mkv"
                 ]
    , testEncode "audios 8,9" (base_req & audios ⊢ AudioTracks (8 :| [9]) )
                 [ "--input"   , "/nonesuch"
                 , "--title"   , "3"
                 , "--markers"
                 , "--deinterlace"
                 , "--audio-copy-mask", "aac,ac3,eac3,truehd,dts,dtshd,mp3,flac"
                 , "--two-pass", "--turbo"
                 , "--preset", "H.265 MKV 2160p60"
                 , "--aencoder","copy"
                 , "--audio"   , "8,9"
                 , "--subtitle", "3,4", "--subtitle-default", "0"
                 , "--quality" , "26.0"
                 , "--output"  , "/03-bob.mkv"
                 ]
    , testEncode "no subs" (base_req & subtitles ⊢ SubtitleTracks [] )
                 [ "--input"   , "/nonesuch"
                 , "--title"   , "3"
                 , "--markers"
                 , "--deinterlace"
                 , "--audio-copy-mask", "aac,ac3,eac3,truehd,dts,dtshd,mp3,flac"
                 , "--two-pass", "--turbo"
                 , "--preset", "H.265 MKV 2160p60"
                 , "--aencoder","copy"
                 , "--audio"   , "2"
                 , "--quality" , "26.0"
                 , "--output"  , "/03-bob.mkv"
                 ]
    , testEncode "quality 22.5" (base_req & quality ⊢ 22.5 )
                 [ "--input"   , "/nonesuch"
                 , "--title"   , "3"
                 , "--markers"
                 , "--deinterlace"
                 , "--audio-copy-mask", "aac,ac3,eac3,truehd,dts,dtshd,mp3,flac"
                 , "--two-pass", "--turbo"
                 , "--preset", "H.265 MKV 2160p60"
                 , "--aencoder","copy"
                 , "--audio"   , "2"
                 , "--subtitle", "3,4", "--subtitle-default", "0"
                 , "--quality" , "22.5"
                 , "--output"  , "/03-bob.mkv"
                 ]
    , testEncode "no audio copy" (base_req & audioEncoder ⊢ 𝕵 "mp3")
                 [ "--input"   , "/nonesuch"
                 , "--title"   , "3"
                 , "--markers"
                 , "--deinterlace"
                 , "--audio-copy-mask", "aac,ac3,eac3,truehd,dts,dtshd,mp3,flac"
                 , "--two-pass", "--turbo"
                 , "--preset", "H.265 MKV 2160p60"
                 , "--aencoder", "mp3"
                 , "--audio"   , "2"
                 , "--subtitle", "3,4", "--subtitle-default", "0"
                 , "--quality" , "26.0"
                 , "--output"  , "/03-bob.mkv"
                 ]
    , testEncode "no name" (base_req & name ⊢ 𝕹)
                 [ "--input"   , "/nonesuch"
                 , "--title"   , "3"
                 , "--markers"
                 , "--deinterlace"
                 , "--audio-copy-mask", "aac,ac3,eac3,truehd,dts,dtshd,mp3,flac"
                 , "--two-pass", "--turbo"
                 , "--preset", "H.265 MKV 2160p60"
                 , "--aencoder", "copy"
                 , "--audio"   , "2"
                 , "--subtitle", "3,4", "--subtitle-default", "0"
                 , "--quality" , "26.0"
                 , "--output"  , "/03.mkv"
                 ]
    , usage_error "no name, no number"
                  "no number & no title"
                  (base_req & name ⊢ 𝕹 & numbering ⊢ NoNumber)
    , usage_error "illegal range"
                  "illegal range «[7-»"
                  (base_req & chapters ⊢ Chapters (𝕵 $ LowerBoundRange (Bound 7 Inclusive)))
    , testEncode "outputDir " (base_req & outputDir ⊢ [absdir|/out/|])
                 [ "--input"   , "/nonesuch"
                 , "--title"   , "3"
                 , "--markers"
                 , "--deinterlace"
                 , "--audio-copy-mask", "aac,ac3,eac3,truehd,dts,dtshd,mp3,flac"
                 , "--two-pass", "--turbo"
                 , "--preset", "H.265 MKV 2160p60"
                 , "--aencoder", "copy"
                 , "--audio"   , "2"
                 , "--subtitle", "3,4", "--subtitle-default", "0"
                 , "--quality" , "26.0"
                 , "--output"  , "/out/03-bob.mkv"
                 ]
    , testEncode "outputName " (base_req & outputName ⊩ [pc|output.mkv|])
                 [ "--input"   , "/nonesuch"
                 , "--title"   , "3"
                 , "--markers"
                 , "--deinterlace"
                 , "--audio-copy-mask", "aac,ac3,eac3,truehd,dts,dtshd,mp3,flac"
                 , "--two-pass", "--turbo"
                 , "--preset", "H.265 MKV 2160p60"
                 , "--aencoder", "copy"
                 , "--audio"   , "2"
                 , "--subtitle", "3,4", "--subtitle-default", "0"
                 , "--quality" , "26.0"
                 , "--output"  , "/output.mkv"
                 ]
    , testEncode "altogether now"
                 (base_req & input ⊢ [absfile|/not-here|]
                           & titleID ⊢ 5
                           & numbering ⊢ Series (7,"T") 1
                           & name ⊢ 𝕹
                           & chapters ⊢ Chapters (𝕵 $ 7 +=+ 9)
                           & twoPass ⊢ NoTwoPass
                           & profile ⊢ ProfileH265_720P
                           & audios ⊢ AudioTracks (2 :| [1])
                           & subtitles ⊢ SubtitleTracks []
                           & quality ⊢ 26
                           & audioEncoder ⊢ 𝕵 "flac24,av_aac"
                           & outputDir ⊢ [absdir|/outdir/|]
                           & outputName ⊩ [pc|out.mkv|])
                 [ "--input"   , "/not-here"
                 , "--title"   , "5"
                 , "--markers"
                 , "--deinterlace"
                 , "--audio-copy-mask", "aac,ac3,eac3,truehd,dts,dtshd,mp3,flac"
                 , "--preset", "H.265 MKV 720p30"
                 , "--aencoder", "flac24,av_aac"
                 , "--audio"   , "2,1"
                 , "--quality" , "26.0"
                 , "--chapters", "7-9"
                 , "--output"  , "/outdir/out.mkv"
                 ]
    , testEncode "dead video"
                 (base_req & input ⊢ [absfile|/not-here|]
                           & titleID ⊢ 5
                           & numbering ⊢ Series (7,"T") 1
                           & name ⊢ 𝕹
                           & chapters ⊢ Chapters (𝕵 $ 7 +=+ 9)
                           & profile ⊢ Profile_DeadVideo
                           & audios ⊢ AudioTracks (2 :| [1])
                           & subtitles ⊢ SubtitleTracks []
                           & quality ⊢ 26
                           & audioEncoder ⊢ 𝕵 "flac24,av_aac"
                           & outputDir ⊢ [absdir|/outdir/|]
                           & outputName ⊩ [pc|out.mkv|])
                 [ "--input"   , "/not-here"
                 , "--title"   , "5"
                 , "--markers"
                 , "--audio-copy-mask", "aac,ac3,eac3,truehd,dts,dtshd,mp3,flac"
                 , "--preset", "H.265 MKV 480p30"
                 , "--aencoder", "flac24,av_aac"
                 , "--audio"   , "2,1"
                 , "--quality" , "26.0"
                 , "--chapters", "7-9"
                 , "--output"  , "/outdir/out.mkv"
                 ]
    ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
