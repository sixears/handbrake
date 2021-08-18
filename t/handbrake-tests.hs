-- base --------------------------------

import System.IO  ( IO )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, defaultMain, testGroup )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified  HandBrake.T.Tests

-------------------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "fmt" [ HandBrake.T.Tests.tests ]

main ∷ IO ()
main = defaultMain tests

-- that's all, folks! ----------------------------------------------------------
