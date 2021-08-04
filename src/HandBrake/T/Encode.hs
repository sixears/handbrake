module HandBrake.T.Encode
  ( tests )
where

-- base --------------------------------

import Control.Applicative  ( pure )
import Data.Function        ( ($), (&) )
import Data.List.NonEmpty   ( NonEmpty( (:|) ) )
import System.Exit          ( ExitCode )
import System.IO            ( IO )

-- fpath -------------------------------

import FPath.AbsFile  ( absfile )
import FPath.File     ( File( FileA, FileR ) )
import FPath.RelFile  ( relfile )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens     ( (⊢), (⊩) )
import Data.MoreUnicode.Maybe    ( pattern 𝕵, pattern 𝕹 )
import Data.MoreUnicode.Natural  ( ℕ )
import Data.MoreUnicode.String   ( 𝕊 )
import Data.MoreUnicode.Text     ( 𝕋 )

-- stdmain -----------------------------

import StdMain.UsageError  ( UsageError, usageError )

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

import HandBrake.Encode  ( AudioCopy( NoAudioCopy ), AudioTracks( AudioTracks )
                         , Numbering( NoNumber, Series )
                         , Profile( ProfileH265_576P, ProfileH265_720P )
                         , TwoPass( NoTwoPass )
                         , audioCopy, audios, chapters, encodeArgs
                         , encodeRequest, input, inputOffset, name, numbering
                         , profile, quality, subtitles, titleID, twoPass
                         )

--------------------------------------------------------------------------------

tests ∷ TestTree
tests =
  testGroup "Encode" $
    let
      testEncode nm req exp =
        testGroup nm $ assertListEqR nm (encodeArgs @UsageError req) exp
      base_req = encodeRequest (FileA [absfile|/nonesuch|]) 3 (𝕵 "bob")
                               (AudioTracks $ pure 2) [3,4]
      usage_error nm txt req =
        testCase nm $
          assertLeft (usageError @𝕋 @UsageError txt ≟) (encodeArgs req)
     in
    [ testEncode "base_req" base_req
                 [ "--input"   , "/nonesuch"
                 , "--title"   , "3"
                 , "--markers"
                 , "--deinterlace"
                 , "--two-pass", "--turbo"
                 , "--preset", "H.265 MKV 2160p60"
                 , "--aencoder", "copy"
                 , "--audio"   , "2"
                 , "--subtitle", "3,4", "--subtitle-default=0"
                 , "--output"  , "03-bob.mkv"
                 ]
    , testEncode "inputOffset 2" (base_req & inputOffset ⊢ 2)
                 [ "--input"   , "/nonesuch"
                 , "--title"   , "3"
                 , "--markers"
                 , "--deinterlace"
                 , "--two-pass", "--turbo"
                 , "--preset", "H.265 MKV 2160p60"
                 , "--aencoder","copy"
                 , "--audio"   , "2"
                 , "--subtitle", "3,4", "--subtitle-default=0"
                 , "--output"  , "05-bob.mkv"
                 ]
    , usage_error "inputOffset -3"
                  "output number 0 (3«-3) < 0" (base_req & inputOffset ⊢ (-3))
    , testEncode "NoNumber" (base_req & numbering ⊢ NoNumber)
                 [ "--input"   , "/nonesuch"
                 , "--title"   , "3"
                 , "--markers"
                 , "--deinterlace"
                 , "--two-pass", "--turbo"
                 , "--preset", "H.265 MKV 2160p60"
                 , "--aencoder","copy"
                 , "--audio"   , "2"
                 , "--subtitle", "3,4", "--subtitle-default=0"
                 , "--output"  , "bob.mkv"
                 ]
    , testEncode "Series S 5" (base_req & numbering ⊢ Series "S" 5)
                 [ "--input"   , "/nonesuch"
                 , "--title"   , "3"
                 , "--markers"
                 , "--deinterlace"
                 , "--two-pass", "--turbo"
                 , "--preset", "H.265 MKV 2160p60"
                 , "--aencoder","copy"
                 , "--audio"   , "2"
                 , "--subtitle", "3,4", "--subtitle-default=0"
                 , "--output"  , "S - 05x03 - bob.mkv"
                 ]
    , testEncode "Series S 6, no name"
                 (base_req & numbering ⊢ Series "S" 6 & name ⊢ 𝕹)
                 [ "--input"   , "/nonesuch"
                 , "--title"   , "3"
                 , "--markers"
                 , "--deinterlace"
                 , "--two-pass", "--turbo"
                 , "--preset", "H.265 MKV 2160p60"
                 , "--aencoder","copy"
                 , "--audio"   , "2"
                 , "--subtitle", "3,4", "--subtitle-default=0"
                 , "--output"  , "S - 06x03.mkv"
                 ]
    , testEncode "chapters 6,7" (base_req & chapters ⊩ [6,7])
                 [ "--input"   , "/nonesuch"
                 , "--title"   , "3"
                 , "--markers"
                 , "--deinterlace"
                 , "--two-pass", "--turbo"
                 , "--preset", "H.265 MKV 2160p60"
                 , "--aencoder","copy"
                 , "--audio"   , "2"
                 , "--subtitle", "3,4", "--subtitle-default=0"
                 , "--chapters", "6,7"
                 , "--output"  , "03-bob.mkv"
                 ]
    , testEncode "no two pass" (base_req & twoPass ⊢ NoTwoPass)
                 [ "--input"   , "/nonesuch"
                 , "--title"   , "3"
                 , "--markers"
                 , "--deinterlace"
                 , "--preset", "H.265 MKV 2160p60"
                 , "--aencoder","copy"
                 , "--audio"   , "2"
                 , "--subtitle", "3,4", "--subtitle-default=0"
                 , "--output"  , "03-bob.mkv"
                 ]
    , testEncode "profile 576" (base_req & profile ⊢ ProfileH265_576P)
                 [ "--input"   , "/nonesuch"
                 , "--title"   , "3"
                 , "--markers"
                 , "--deinterlace"
                 , "--two-pass", "--turbo"
                 , "--preset", "H.265 MKV 576p25"
                 , "--aencoder","copy"
                 , "--audio"   , "2"
                 , "--subtitle", "3,4", "--subtitle-default=0"
                 , "--output"  , "03-bob.mkv"
                 ]
    , testEncode "audios 8,9" (base_req & audios ⊢ AudioTracks (8 :| [9]) )
                 [ "--input"   , "/nonesuch"
                 , "--title"   , "3"
                 , "--markers"
                 , "--deinterlace"
                 , "--two-pass", "--turbo"
                 , "--preset", "H.265 MKV 2160p60"
                 , "--aencoder","copy"
                 , "--audio"   , "8,9"
                 , "--subtitle", "3,4", "--subtitle-default=0"
                 , "--output"  , "03-bob.mkv"
                 ]
    , testEncode "no subs" (base_req & subtitles ⊢ [] )
                 [ "--input"   , "/nonesuch"
                 , "--title"   , "3"
                 , "--markers"
                 , "--deinterlace"
                 , "--two-pass", "--turbo"
                 , "--preset", "H.265 MKV 2160p60"
                 , "--aencoder","copy"
                 , "--audio"   , "2"
                 , "--output"  , "03-bob.mkv"
                 ]
    , testEncode "quality 22.5" (base_req & quality ⊩ 22.5 )
                 [ "--input"   , "/nonesuch"
                 , "--title"   , "3"
                 , "--markers"
                 , "--deinterlace"
                 , "--two-pass", "--turbo"
                 , "--preset", "H.265 MKV 2160p60"
                 , "--aencoder","copy"
                 , "--audio"   , "2"
                 , "--subtitle", "3,4", "--subtitle-default=0"
                 , "--quality" , "22.5"
                 , "--output"  , "03-bob.mkv"
                 ]
    , testEncode "no audio copy" (base_req & audioCopy ⊢ NoAudioCopy)
                 [ "--input"   , "/nonesuch"
                 , "--title"   , "3"
                 , "--markers"
                 , "--deinterlace"
                 , "--two-pass", "--turbo"
                 , "--preset", "H.265 MKV 2160p60"
                 , "--audio"   , "2"
                 , "--subtitle", "3,4", "--subtitle-default=0"
                 , "--output"  , "03-bob.mkv"
                 ]
    , testEncode "no name" (base_req & name ⊢ 𝕹)
                 [ "--input"   , "/nonesuch"
                 , "--title"   , "3"
                 , "--markers"
                 , "--deinterlace"
                 , "--two-pass", "--turbo"
                 , "--preset", "H.265 MKV 2160p60"
                 , "--aencoder", "copy"
                 , "--audio"   , "2"
                 , "--subtitle", "3,4", "--subtitle-default=0"
                 , "--output"  , "03.mkv"
                 ]
    , usage_error "no name, no number"
                  "no number & no title"
                  (base_req & name ⊢ 𝕹 & numbering ⊢ NoNumber)
    , testEncode "altogether now"
                 (base_req & input ⊢ FileR [relfile|not-here|]
                           & titleID ⊢ 5
                           & inputOffset ⊢ 1
                           & numbering ⊢ Series "T" 7
                           & name ⊢ 𝕹
                           & chapters ⊩ [8,9]
                           & twoPass ⊢ NoTwoPass
                           & profile ⊢ ProfileH265_720P
                           & audios ⊢ AudioTracks (2 :| [1])
                           & subtitles ⊢ []
                           & quality ⊩ 26
                           & audioCopy ⊢ NoAudioCopy)
                 [ "--input"   , "not-here"
                 , "--title"   , "5"
                 , "--markers"
                 , "--deinterlace"
                 , "--preset", "H.265 MKV 720p30"
                 , "--audio"   , "2,1"
                 , "--quality" , "26.0"
                 , "--chapters", "8,9"
                 , "--output"  , "T - 07x06.mkv"
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
