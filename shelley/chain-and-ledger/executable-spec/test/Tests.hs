import           Test.Tasty

import           PropertyTests (propertyTests)
import           STSTests (stsTests)
import           Test.Serialization (serializationTests)
import           UnitTests (unitTests)

ciTests :: TestTree
ciTests = testGroup "CI_TESTS"
  [ unitTests
  , stsTests
  , serializationTests
  ]

nightlyTests :: TestTree
nightlyTests = testGroup "NIGHTLY_TESTS"
  [ propertyTests
  ]

tests :: TestTree
tests = testGroup "Ledger with Delegation"
  [ ciTests
  , nightlyTests
  ]

-- main entry point
main :: IO ()
main = defaultMain tests
