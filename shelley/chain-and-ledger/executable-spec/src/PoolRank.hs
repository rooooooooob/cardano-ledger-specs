module PoolRank
  ( desirability
  , rankedPools
  ) where

import           BaseTypes (intervalValue)
import           Coin (Coin (..))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Ratio ((%))
import qualified Data.Set as Set
import           EpochBoundary (BlocksMade (..), SnapShots (..), Stake (..), poolStake)
import           Keys (KeyHash)
import           LedgerState (AccountState (..), EpochState (..), NewEpochState (..))
import           Numeric.Natural (Natural)
import           PParams (PParams (..))
import           Slot (EpochSize)
import           TxData (PoolParams (..))


-- | Desirability calculation for non-myopic utily,
-- corresponding to f^~ in section 5.6.1 of
-- "Design Specification for Delegation and Incentives in Cardano"
desirability
  :: PParams
  -> Coin
  -> Natural
  -> Natural
  -> PoolParams crypto
  -> Stake crypto
  -> Coin
  -> Rational
desirability pp r blocksN blocksTotal pool (Stake stake) (Coin total) =
  if fTilde <= cost
    then 0
    else (fTilde - cost)*(1 -margin)
  where
    fTilde      = fTildeNumer / fTildeDenom
    fTildeNumer = pBar * fromIntegral r * (z0 + min s z0 * a0)
    fTildeDenom = 1 + a0

    cost   = fromIntegral $ _poolCost pool
    margin = intervalValue $ _poolMargin pool
    tot = max 1 (fromIntegral total)

    Coin pledge = _poolPledge pool
    s = fromIntegral pledge % tot
    a0 = _a0 pp
    z0 = 1 % max 1 (fromIntegral (_nOpt pp))

    pBar = if intervalValue (_d pp) < 0.8 && sigma > 0
             then beta / sigma
             else 1
    Coin pstake = sum stake
    sigma = fromIntegral pstake % tot
    beta = fromIntegral blocksN / fromIntegral (max 1 blocksTotal)

-- | Computes the top ranked stake pools
-- corresponding to section 5.6.1 of
-- "Design Specification for Delegation and Incentives in Cardano"
rankedPools
  :: NewEpochState crypto
  -> EpochSize
  -> Map (KeyHash crypto) Rational
rankedPools nes slotsPerEpoch = rankings
  where
    EpochState acnt ss _ pp = nesEs nes
    poolParams = _poolsSS ss
    (stake@(Stake stake'), delegs) = _pstakeGo ss
    (BlocksMade b) = nesBprev nes

    -- compute r
    Coin reserves = _reserves acnt
    blocksMade = fromIntegral $ sum b :: Integer
    expectedBlocks = intervalValue (_activeSlotCoeff pp) * fromIntegral slotsPerEpoch
    eta = fromIntegral blocksMade / min 1 expectedBlocks
    r = floor $ min 1 eta * intervalValue (_rho pp) * fromIntegral reserves

    pdata =
      [ ( hk
        , ( poolParams Map.! hk
          , b Map.! hk
          , poolStake hk delegs stake))
      | hk <-
          Set.toList $ Map.keysSet poolParams `Set.intersection` Map.keysSet b
      ]
    rankings = Map.fromList
      [ ( hk
        , desirability pp r n totalBlocks pool pstake total)
      | (hk, (pool, n, pstake)) <- pdata
      ]
    total = sum stake'
    totalBlocks = sum b
