{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

-- | This module contains just the type of protocol parameters.
module PParams
  ( PParams(..)
  , emptyPParams
  -- lenses
  , minfeeA
  , minfeeB
  , maxBBSize
  , maxTxSize
  , maxBHSize
  , keyDeposit
  , keyMinRefund
  , keyDecayRate
  , poolDeposit
  , poolMinRefund
  , poolDecayRate
  , eMax
  , nOpt
  , a0
  , rho
  , tau
  , activeSlotCoeff
  , d
  , extraEntropy
  , protocolVersion
  , ActiveSlotCoeff
  , mkActiveSlotCoeff
  , activeSlotVal
  , activeSlotLog
  ) where

import           Cardano.Binary (FromCBOR (fromCBOR), ToCBOR (toCBOR), decodeListLen, decodeWord,
                     encodeListLen, enforceSize, matchSize)
import           BaseTypes (invalidKey)
import           Cardano.Prelude (NoUnexpectedThunks (..))
import           GHC.Generics (Generic)
import           Numeric.Natural (Natural)
import           Cardano.Ledger.Shelley.Crypto
import           Cardano.Crypto.Hash (Hash, hash)
import           Data.Word (Word8)
import           Data.Set (Set)
import           Data.Map.Strict (Map, insert, empty, findWithDefault)
import           BaseTypes (FixedPoint, Nonce (NeutralNonce), UnitInterval, fpPrecision, interval0,
                     intervalValue)
import           Coin (Coin (..))
import           Slot (EpochNo (..))

import qualified NonIntegral (ln')

import           Lens.Micro.TH (makeLenses)
import           CostModel
import           Scripts

-- | Protocol parameters
data PParams = PParams
  { -- |The linear factor for the minimum fee calculation
    _minfeeA         :: Integer
    -- |The constant factor for the minimum fee calculation
  , _minfeeB         :: Natural
    -- | Maximal block body size
  , _maxBBSize       :: Natural
    -- | Maximal transaction size
  , _maxTxSize       :: Natural
    -- | Maximal block header size
  , _maxBHSize       :: Natural
    -- |The amount of a key registration deposit
  , _keyDeposit      :: Coin
    -- |The minimum percent refund guarantee
  , _keyMinRefund    :: UnitInterval
    -- |The deposit decay rate
  , _keyDecayRate    :: Rational
    -- |The amount of a pool registration deposit
  , _poolDeposit     :: Coin
    -- | The minimum percent pool refund
  , _poolMinRefund   :: UnitInterval
    -- | Decay rate for pool deposits
  , _poolDecayRate   :: Rational
    -- | epoch bound on pool retirement
  , _eMax            :: EpochNo
    -- | Desired number of pools
  , _nOpt            :: Natural
    -- | Pool influence
  , _a0              :: Rational
    -- | Treasury expansion
  , _rho             :: UnitInterval
    -- | Monetary expansion
  , _tau             :: UnitInterval
    -- | Active slot coefficient
  , _activeSlotCoeff :: ActiveSlotCoeff
    -- | Decentralization parameter
  , _d               :: UnitInterval
    -- | Extra entropy
  , _extraEntropy    :: Nonce
    -- | Protocol version
  , _protocolVersion :: (Natural, Natural)
    -- | cost models for each language that uses them
  , _costmdls        :: CostModels
    -- | Prices of execution units
  , _prices          :: Prices
    -- | exunits limit per transaction
  , _maxTxExUnits    :: ExUnits
    -- | exunits limit per block
  , _maxBlockExUnits :: ExUnits
  } deriving (Show, Eq, Generic)

data ActiveSlotCoeff =
  ActiveSlotCoeff
  { unActiveSlotVal :: UnitInterval
  , unActiveSlotLog :: Integer  -- TODO mgudemann make this FixedPoint,
                                -- currently a problem because of
                                -- NoUnexpectedThunks instance for FixedPoint
  } deriving (Eq, Ord, Show, Generic)

instance NoUnexpectedThunks ActiveSlotCoeff

instance FromCBOR ActiveSlotCoeff
 where
   fromCBOR = do
     v <- fromCBOR
     -- l <_ fromCBOR TODO unActiveSlotLog is currently ignored
     pure $ mkActiveSlotCoeff v

instance ToCBOR ActiveSlotCoeff
 where
   toCBOR (ActiveSlotCoeff { unActiveSlotVal = slotVal
                           , unActiveSlotLog = _logVal}) = -- TODO unActiveSlotLog is currently ignored
     toCBOR slotVal

mkActiveSlotCoeff :: UnitInterval -> ActiveSlotCoeff
mkActiveSlotCoeff v =
  ActiveSlotCoeff { unActiveSlotVal = v
                  , unActiveSlotLog = floor
                    (fpPrecision * (
                        NonIntegral.ln' $
                        (1 :: FixedPoint) - (fromRational $ intervalValue v))) }

activeSlotVal :: ActiveSlotCoeff -> UnitInterval
activeSlotVal = unActiveSlotVal

activeSlotLog :: ActiveSlotCoeff -> FixedPoint
activeSlotLog f = (fromIntegral $ unActiveSlotLog f) / fpPrecision

-- hash of the parameters relevant to a language
data PPHashItems crypto =
  -- PLCV1 only needs hashes of the cost model for this language
  PLCV1PPHash CostMod
  -- hashes of parameters for other languages go here
  deriving (Show, Eq, Generic)

data PPHash crypto = PPHash (Hash (HASH crypto) (Map Language (PPHashItems crypto)))
  deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (PPHash crypto)

instance NoUnexpectedThunks (PPHashItems crypto)

-- | helper function adds the items needed to hash for each language in a set
mkMap :: (Map Language CostMod)
  -> (Map Language (PPHashItems crypto))
  -> Language -> (Map Language (PPHashItems crypto))
mkMap cm oldm k
  | k == Language plcV1 = insert k (PLCV1PPHash (findWithDefault defaultModel k cm)) oldm

-- | hash parameters relevant to languages in the set
hashLanguagePP :: Crypto crypto
  => PParams
  -> (Set Language)
  -> Maybe (PPHash crypto)
hashLanguagePP pp ls
    | null ls       = Nothing
    | not $ null ls = Just $ PPHash (hash (foldl (mkMap cm) Data.Map.Strict.empty ls))
        where (CostModels cm) = _costmdls pp

instance NoUnexpectedThunks PParams

instance ToCBOR PParams
 where
  toCBOR (PParams
    { _minfeeA         = minfeeA'
    , _minfeeB         = minfeeB'
    , _maxBBSize       = maxBBSize'
    , _maxTxSize       = maxTxSize'
    , _maxBHSize       = maxBHSize'
    , _keyDeposit      = keyDeposit'
    , _keyMinRefund    = keyMinRefund'
    , _keyDecayRate    = keyDecayRate'
    , _poolDeposit     = poolDeposit'
    , _poolMinRefund   = poolMinRefund'
    , _poolDecayRate   = poolDecayRate'
    , _eMax            = eMax'
    , _nOpt            = nOpt'
    , _a0              = a0'
    , _rho             = rho'
    , _tau             = tau'
    , _activeSlotCoeff = activeSlotCoeff'
    , _d               = d'
    , _extraEntropy    = extraEntropy'
    , _protocolVersion = protocolVersion'
    , _costmdls        = costmdls'
    , _prices          = prices'
    , _maxTxExUnits    = maxTxExUnits'
    , _maxBlockExUnits = maxBlockExUnits'
    }) =
      encodeListLen 24
        <> toCBOR minfeeA'
        <> toCBOR minfeeB'
        <> toCBOR maxBBSize'
        <> toCBOR maxTxSize'
        <> toCBOR maxBHSize'
        <> toCBOR keyDeposit'
        <> toCBOR keyMinRefund'
        <> toCBOR keyDecayRate'
        <> toCBOR poolDeposit'
        <> toCBOR poolMinRefund'
        <> toCBOR poolDecayRate'
        <> toCBOR eMax'
        <> toCBOR nOpt'
        <> toCBOR a0'
        <> toCBOR rho'
        <> toCBOR tau'
        <> toCBOR activeSlotCoeff'
        <> toCBOR d'
        <> toCBOR extraEntropy'
        <> toCBOR protocolVersion'
        <> toCBOR costmdls'
        <> toCBOR prices'
        <> toCBOR maxTxExUnits'
        <> toCBOR maxBlockExUnits'

instance FromCBOR PParams
 where
  fromCBOR = do
    enforceSize "PParams" 24
    PParams
      <$> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR

makeLenses ''PParams

-- | Returns a basic "empty" `PParams` structure with all zero values.
emptyPParams :: PParams
emptyPParams =
    PParams {
       _minfeeA = 0
     , _minfeeB = 0
     , _maxBBSize = 0
     , _maxTxSize = 2048
     , _maxBHSize = 0
     , _keyDeposit = Coin 0
     , _keyMinRefund = interval0
     , _keyDecayRate = 0
     , _poolDeposit = Coin 0
     , _poolMinRefund = interval0
     , _poolDecayRate = 0
     , _eMax = EpochNo 0
     , _nOpt = 100
     , _a0 = 0
     , _rho = interval0
     , _tau = interval0
     , _activeSlotCoeff = mkActiveSlotCoeff interval0
     , _d = interval0
     , _extraEntropy = NeutralNonce
     , _protocolVersion = (0, 0)
     , _costmdls = defaultModels
     , _prices = defaultPrices
     , _maxTxExUnits = defaultUnits
     , _maxBlockExUnits = defaultUnits
     }

-- | CBOR

-- | numbers correspond to language tags (plcV1 = 1)
instance
  (Crypto crypto)
  => ToCBOR (PPHashItems crypto)
 where
   toCBOR = \case
     PLCV1PPHash cm ->
           encodeListLen 2
           <> toCBOR (1 :: Word8)
           <> toCBOR cm
-- new languages go here

instance
  (Crypto crypto)
  => FromCBOR (PPHashItems crypto)
 where
  fromCBOR = do
    n <- decodeListLen
    decodeWord >>= \case
      0 -> matchSize "PLCV1PPHash" 2 n >> PLCV1PPHash <$> fromCBOR
-- new languages go here
      k -> invalidKey k
