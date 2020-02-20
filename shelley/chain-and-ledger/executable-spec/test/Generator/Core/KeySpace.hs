
module Generator.Core.KeySpace 
  ( KeySpace (..)
  , keySpace
  )
  where

import Cardano.Prelude

import ConcreteCryptoTypes (KeyPair, MultiSig, CoreKeyPair, SignKeyVRF, VerKeyVRF, AnyKeyHash, KeyHash)


data KeySpace = KeySpace
  { stakingKeys :: [KeyPair]
  , spendingKeys :: [KeyPair]
  , multiSigPairs :: [(MultiSig, MultiSig)]
  , coreKeyPairs :: [CoreKeyPair]
  , vrfSigningKeys :: [(SignKeyVRF, VerKeyVRF)]
  , stakingKeyLookup :: Map KeyHash KeyPair
    -- ^ keys: hashes of staking keys
    -- ^ values: full staking key pair
  , traceKeyHashMap :: Map AnyKeyHash KeyPair
    -- ^ keys: hashes of multisig scripts
    -- ^ values: signing key of the multisig script
  }

keySpace :: KeySpace
keySpace = undefined

getAvailableKeyPairPair :: ((KeyPair, Keypair) -> Bool) -> KeySpace -> [(KeyPair,KeyPair)]
getAvailableKeyPairPair predicate keySpace = undefined

getAvailableMultiSig :: ([(MultiSig, MultiSig)] -> Bool) -> KeySpace -> [(MultiSig, MultiSig)]
getAvailableMultiSig predicate keySpace = undefined

getAnyStakeVKey :: KeySpace -> Gen VKey
getAnyStakeVKey keySpace = undefined







