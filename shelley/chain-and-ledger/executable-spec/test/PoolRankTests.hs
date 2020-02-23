{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}

module PoolRankTests (poolRankTests) where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Utils

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Word (Word64)
import           Numeric.Natural (Natural)

import           BaseTypes (UnitInterval (..))
import           Coin (Coin (..))
import           ConcreteCryptoTypes (EpochState, KeyHash, NewEpochState, PoolParams, hashKeyVRF)
import           Delegation.Certificates (PoolDistr (..))
import           EpochBoundary (BlocksMade (..), pattern Stake, emptySnapShots, _poolsSS, _pstakeGo)
import           Keys (hashKey)
import           LedgerState (AccountState (..), pattern EpochState, pattern NewEpochState,
                     emptyLedgerState, esAccountState, esLState, esPp, esSnapshots, nesBprev,
                     nesEs)
import           PoolRank (rankedPools)
import           PParams (PParams (..), emptyPParams)
import           Slot (EpochNo (..), EpochSize (..))
import           TxData (pattern KeyHashObj, pattern PoolParams, pattern RewardAcnt, _poolCost,
                     _poolMargin, _poolOwners, _poolPledge, _poolPubKey, _poolRAcnt, _poolVrf)

pp :: PParams
pp = emptyPParams
       { _d               = unsafeMkUnitInterval 0.5
       , _activeSlotCoeff = unsafeMkUnitInterval 0.1
       , _rho             = unsafeMkUnitInterval 0.002
       , _a0              = 0.1
       , _nOpt            = 100
       }

mkOperatorHash :: Word64 -> KeyHash
mkOperatorHash w = hashKey $ snd $ mkKeyPair (0, 0, 0, 0, w)

mkDelegatorHash :: Word64 -> Word64 -> KeyHash
mkDelegatorHash v w = hashKey $ snd $ mkKeyPair (0, 0, 0, v, w)

data Pool =
  Pool
    { poolId  :: Word64
    , pledge :: Coin
    , cost   :: Coin
    , margin :: UnitInterval
    , blocks :: Natural
    , delegs :: [Coin]
    }

mkPoolParams
  :: Pool
  -> PoolParams
mkPoolParams pool =
  PoolParams
    { _poolPubKey = mkOperatorHash $ poolId pool
    , _poolPledge = pledge pool
    , _poolCost = cost pool
    , _poolMargin = margin pool

    -- the values below are not used in ranking
    , _poolVrf = (hashKeyVRF . snd) (mkVRFKeyPair (0,0,0,0,0))
    , _poolRAcnt = (RewardAcnt . KeyHashObj) (mkOperatorHash 0)
    , _poolOwners = Set.empty
    }

es0 :: EpochState
es0 = EpochState
         { esAccountState = AccountState
             { _treasury = Coin 0
             , _reserves = Coin 1000000
             }
         , esSnapshots = emptySnapShots
         , esLState = emptyLedgerState
         , esPp = pp
         }

addPool :: Pool -> NewEpochState -> NewEpochState
addPool pool nes = nes
  { nesEs = es
      { esSnapshots = ss
            { _pstakeGo = ( Stake $ stk `Map.union` newStk
                          , dels `Map.union` newDels
                          )
            , _poolsSS = Map.insert opHash (mkPoolParams pool) pools
            }
      }
  , nesBprev = BlocksMade $ Map.insert opHash (blocks pool) bsOld
  }
  where
    opHash = mkOperatorHash $ poolId pool
    es = nesEs nes
    ss = esSnapshots es
    (Stake stk, dels) = _pstakeGo ss
    pools = _poolsSS ss
    (BlocksMade bsOld) = nesBprev nes

    delegators = fmap
      (KeyHashObj . mkDelegatorHash (poolId pool) . fromIntegral)
      [1..length (delegs pool)]
    newStk = Map.fromList $ zip delegators (delegs pool)
    newDels = Map.fromList $ fmap (, opHash) delegators

nes0 :: NewEpochState
nes0 = NewEpochState
         (EpochNo 0)
         (BlocksMade Map.empty)
         (BlocksMade Map.empty)
         es0
         Nothing
         (PoolDistr Map.empty)
         Map.empty

addPools :: NewEpochState -> [Pool] -> NewEpochState
addPools = foldr addPool

esize :: EpochSize
esize = EpochSize 100

alice :: Pool
alice = Pool
  { poolId  = 1
  , pledge = Coin 5
  , cost   = Coin 15
  , margin = unsafeMkUnitInterval 0.1
  , blocks = 10
  , delegs = [Coin 10]
  }

testNoPools :: Assertion
testNoPools =
  let
    rs = rankedPools nes0 esize
  in rs @?= Map.empty

getRank
  :: Map KeyHash Rational
  -> Pool
  -> Rational
getRank rankings pool = rankings Map.! mkOperatorHash (poolId pool)

testAliceRankedBetter :: Pool -> Assertion
testAliceRankedBetter bob =
  let
    rank = getRank $ rankedPools (addPools nes0 [alice, bob]) esize
  in rank alice > rank bob @? "alice was not ranked better"

testIncreaseCost :: Assertion
testIncreaseCost = testAliceRankedBetter alice
  { poolId = 2 , cost = cost alice + 1 }

testIncreaseMargin :: Assertion
testIncreaseMargin = testAliceRankedBetter alice
  { poolId = 2 , margin = unsafeMkUnitInterval 0.2 }

testDecreasePledge :: Assertion
testDecreasePledge = testAliceRankedBetter alice
  { poolId = 2, pledge = pledge alice - 10 }

testDecreasePerformance :: Assertion
testDecreasePerformance = testAliceRankedBetter alice
  { poolId = 2 , blocks = blocks alice - 5 }

poolRankTests :: TestTree
poolRankTests = testGroup "Pool Rank Tests"
  [ testCase "no pools" testNoPools
  -- TODO turn the tests below into property tests
  , testCase "increase cost" testIncreaseCost
  , testCase "increase margin" testIncreaseMargin
  , testCase "increase pledge" testDecreasePledge
  , testCase "increase performance" testDecreasePerformance
  ]
