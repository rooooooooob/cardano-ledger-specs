{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Test.Shelley.Spec.Ledger.Rewards (rewardTests) where

import qualified Cardano.Crypto.DSIGN as Crypto
import Cardano.Crypto.Hash (Hash (UnsafeHash), MD5, ShortHash, hash)
import Cardano.Crypto.Seed (mkSeedFromBytes)
import qualified Cardano.Crypto.VRF as Crypto
import Cardano.Slotting.Slot (EpochSize (..))
import Control.Monad (replicateM)
import Data.Coerce (coerce)
import Data.Foldable (fold)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ratio (Ratio, (%))
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Data.Word (Word64)
import Numeric.Natural (Natural)
import Shelley.Spec.Ledger.BaseTypes
  ( Globals (..),
    Network (..),
    StrictMaybe (..),
    UnitInterval (..),
    mkActiveSlotCoeff,
  )
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.Credential (Credential (..))
import Shelley.Spec.Ledger.Crypto (Crypto (..))
import Shelley.Spec.Ledger.EpochBoundary
  ( BlocksMade (..),
    Stake (..),
  )
import Shelley.Spec.Ledger.Keys
  ( KeyPair (..),
    KeyRole (..),
    VKey (..),
    hashKey,
    vKey,
  )
import Shelley.Spec.Ledger.PParams
  ( PParams,
    PParams' (..),
    emptyPParams,
  )
import Shelley.Spec.Ledger.Rewards (reward)
import Shelley.Spec.Ledger.TxData (PoolParams (..), RewardAcnt (..))
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (ConcreteCrypto)
import Test.Shelley.Spec.Ledger.Generator.Core (genCoin, genNatural)
import Test.Shelley.Spec.Ledger.Utils
  ( testGlobals,
    unsafeMkUnitInterval,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
  ( Gen,
    Property,
    arbitrary,
    choose,
    counterexample,
    elements,
    property,
    testProperty,
    withMaxSuccess,
  )

-- Bounds and Constants --

maxNumPools :: Int
maxNumPools = 100

maxNumMembers :: Int
maxNumMembers = 100

maxMemberLovelace :: Integer
maxMemberLovelace = 100000

maxOwnerLovelaceAbovePledge :: Integer
maxOwnerLovelaceAbovePledge = 100000

maxPoolPledeg :: Integer
maxPoolPledeg = 1000000

maxPoolCost :: Integer
maxPoolCost = 1000000

maxPoolBlocks :: Natural
maxPoolBlocks = 1000000

numberOfTests :: Int
numberOfTests = 500

decentralizationRange :: [Ratio Word64]
decentralizationRange = [0, 0.1 .. 1]

tauRange :: [Ratio Word64]
tauRange = [0, 0.05 .. 0.3]

rhoRange :: [Ratio Word64]
rhoRange = [0, 0.05 .. 0.3]

-- Helpers --

keyPair :: Crypto c => Int -> KeyPair r c
keyPair seed = KeyPair vk sk
  where
    vk = VKey (Crypto.deriveVerKeyDSIGN sk)
    sk = Crypto.genKeyDSIGN $ mkSeedFromBytes . coerce $ hash @MD5 seed

vrfKeyPair :: forall v. Crypto.VRFAlgorithm v => Int -> (Crypto.SignKeyVRF v, Crypto.VerKeyVRF v)
vrfKeyPair seed = (sk, vk)
  where
    vk = Crypto.deriveVerKeyVRF sk
    sk = Crypto.genKeyVRF $ mkSeedFromBytes . coerce $ hash @MD5 seed

data PoolSetUpArgs crypto f = PoolSetUpArgs
  { poolPledge :: f Coin,
    poolCost :: f Coin,
    poolMargin :: f UnitInterval,
    poolMembers :: f (Map (Credential 'Staking crypto) Coin)
  }

emptySetupArgs :: PoolSetUpArgs crypto Maybe
emptySetupArgs =
  PoolSetUpArgs
    { poolPledge = Nothing,
      poolCost = Nothing,
      poolMargin = Nothing,
      poolMembers = Nothing
    }

data PoolInfo crypto = PoolInfo
  { params :: PoolParams crypto,
    coldKey :: KeyPair 'StakePool crypto,
    ownerKey :: KeyPair 'Staking crypto,
    ownerStake :: Coin,
    rewardKey :: KeyPair 'Staking crypto,
    members :: Map (Credential 'Staking crypto) Coin
  }

-- Generators --

genNonOwnerMembers :: Crypto c => Gen (Map (Credential 'Staking c) Coin)
genNonOwnerMembers = do
  numMembers <- choose (0, maxNumMembers)
  fmap Map.fromList . replicateM numMembers $ do
    credential <- KeyHashObj . hashKey . vKey . keyPair <$> arbitrary
    coins <- genCoin 0 maxMemberLovelace
    pure (credential, coins)

getOrGen :: Maybe a -> Gen a -> Gen a
getOrGen (Just x) _ = pure x
getOrGen Nothing g = g

genMargin :: Gen UnitInterval
genMargin = do
  let denom = 10
  numer <- choose (0, denom)
  pure $ unsafeMkUnitInterval (numer % denom)

genPoolInfo :: forall c. Crypto c => PoolSetUpArgs c Maybe -> Gen (PoolInfo c)
genPoolInfo PoolSetUpArgs {poolPledge, poolCost, poolMargin, poolMembers} = do
  pledge <- getOrGen poolPledge $ genCoin 0 maxPoolPledeg
  cost <- getOrGen poolCost $ genCoin 0 maxPoolCost
  margin <- getOrGen poolMargin genMargin
  vrfKey <- vrfKeyPair @(VRF c) <$> arbitrary
  coldKey <- keyPair <$> arbitrary
  ownerKey <- keyPair <$> arbitrary
  rewardKey <- keyPair <$> arbitrary
  members' <- getOrGen poolMembers genNonOwnerMembers
  ownerStake <- (pledge +) <$> genCoin 0 maxOwnerLovelaceAbovePledge
  -- here we are forcing the pool to meet the pledeg, later we may want flexibility
  let members = Map.insert (KeyHashObj . hashKey . vKey $ ownerKey) ownerStake members'
      params =
        PoolParams
          { _poolPubKey = hashKey . vKey $ coldKey,
            _poolVrf = Crypto.hashVerKeyVRF . snd $ vrfKey,
            _poolPledge = pledge,
            _poolCost = cost,
            _poolMargin = margin,
            _poolRAcnt = RewardAcnt Testnet . KeyHashObj . hashKey . vKey $ rewardKey,
            _poolOwners = Set.fromList [hashKey $ vKey ownerKey],
            _poolRelays = StrictSeq.empty,
            _poolMD = SNothing
          }
  pure $ PoolInfo {params, coldKey, ownerKey, ownerStake, rewardKey, members}

genRewardPPs :: Gen PParams
genRewardPPs = do
  d <- g decentralizationRange
  t <- g tauRange
  r <- g rhoRange
  pure $ emptyPParams {_d = d, _tau = t, _rho = r}
  where
    g xs = unsafeMkUnitInterval <$> elements xs

genBlocksMade :: [PoolParams crypto] -> Gen (BlocksMade crypto)
genBlocksMade pools = BlocksMade . Map.fromList <$> mapM f pools
  where
    f p = (_poolPubKey p,) <$> genNatural 0 maxPoolBlocks

-- Properties --

type C = ConcreteCrypto ShortHash

rewardsBoundedByPot :: Property
rewardsBoundedByPot = property $ do
  numPools <- choose (0, maxNumPools)
  pools <- sequence $ genPoolInfo @C <$> replicate numPools emptySetupArgs
  pp <- genRewardPPs
  rewardPot <- genCoin 0 (fromIntegral $ maxLovelaceSupply testGlobals)
  undelegatedLovelace <- genCoin 0 (fromIntegral $ maxLovelaceSupply testGlobals)
  asc <- mkActiveSlotCoeff . unsafeMkUnitInterval <$> elements [0.1, 0.2, 0.3]
  bs@(BlocksMade blocks) <- genBlocksMade (fmap params pools)
  let totalBlocks = sum blocks
  silentSlots <- genNatural 0 (3 * totalBlocks) -- the '3 * sum blocks' is pretty arbitrary
  let stake = fold (members <$> pools)
      delegs = fold $
        flip fmap pools $ \PoolInfo {params, members} ->
          Map.fromList $ (,_poolPubKey params) <$> Map.keys members
      rewardAcnts = Set.fromList $ fmap (RewardAcnt Testnet) (Map.keys delegs)
      poolParams =
        Map.fromList $
          fmap
            ( \PoolInfo {params} ->
                (_poolPubKey params, params)
            )
            pools
      totalLovelace = undelegatedLovelace + sum stake
      slotsPerEpoch = EpochSize . fromIntegral $ totalBlocks + silentSlots
      rs =
        reward
          Testnet
          pp
          bs
          rewardPot
          rewardAcnts
          poolParams
          (Stake stake)
          delegs
          totalLovelace
          asc
          slotsPerEpoch
  pure $
    counterexample
      ( mconcat
          [ "pp\n",
            show pp,
            "\nrewardPot\n",
            show rewardPot,
            "\nrewardAcnts\n",
            show rewardAcnts,
            "\npoolParams\n",
            show poolParams,
            "\nstake\n",
            show stake,
            "\ndelegs\n",
            show delegs,
            "\ntotalLovelace\n",
            show totalLovelace,
            "\nasc\n",
            show asc,
            "\nslotsPerEpoch\n",
            show slotsPerEpoch
          ]
      )
      (sum (fst rs) < rewardPot)

rewardTests :: TestTree
rewardTests =
  testGroup
    "Reward Tests"
    [ testProperty
        "Sum of rewards is bounded by reward pot"
        (withMaxSuccess numberOfTests rewardsBoundedByPot)
    ]
