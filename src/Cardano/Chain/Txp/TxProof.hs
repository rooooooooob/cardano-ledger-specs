{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

module Cardano.Chain.Txp.TxProof
  ( TxProof(..)
  , mkTxProof
  , recoverTxProof
  )
where

import Cardano.Prelude

import Formatting (bprint, build)
import qualified Formatting.Buildable as B

import Cardano.Binary (FromCBOR(..), ToCBOR(..), encodeListLen, enforceSize)
import Cardano.Chain.Common.Merkle (MerkleRoot, mkMerkleTree, mtRoot)
import Cardano.Chain.Txp.Tx (Tx)
import Cardano.Chain.Txp.TxPayload
  (ATxPayload, TxPayload, recoverHashedBytes, txpTxs, txpWitnesses)
import Cardano.Chain.Txp.TxWitness (TxWitness)
import Cardano.Crypto (Hash, hash, hashDecoded)


data TxProof = TxProof
  { txpNumber        :: !Word32
  , txpRoot          :: !(MerkleRoot Tx)
  , txpWitnessesHash :: !(Hash [TxWitness])
  } deriving (Show, Eq, Generic)
    deriving anyclass NFData

instance B.Buildable TxProof where
  build proof = bprint
    ("<TxProof: " . build . ", " . build . ", " . build . ">")
    (txpNumber proof)
    (txpRoot proof)
    (txpWitnessesHash proof)

instance ToCBOR TxProof where
  toCBOR proof =
    encodeListLen 3
      <> toCBOR (txpNumber proof)
      <> toCBOR (txpRoot proof)
      <> toCBOR (txpWitnessesHash proof)

instance FromCBOR TxProof where
  fromCBOR = do
    enforceSize "TxProof" 3
    TxProof <$> fromCBOR <*> fromCBOR <*> fromCBOR

-- | Construct 'TxProof' which proves given 'TxPayload'
--
--   This will construct a Merkle tree, which can be very expensive. Use with
--   care.
mkTxProof :: TxPayload -> TxProof
mkTxProof payload = TxProof
  { txpNumber        = fromIntegral (length $ txpTxs payload)
  , txpRoot          = mtRoot (mkMerkleTree $ txpTxs payload)
  , txpWitnessesHash = hash $ txpWitnesses payload
  }

recoverTxProof :: ATxPayload ByteString -> TxProof
recoverTxProof payload = TxProof
  { txpNumber        = fromIntegral (length $ txpTxs payload)
  , txpRoot          = mtRoot (mkMerkleTree $ txpTxs payload)
  , txpWitnessesHash = hashDecoded $ recoverHashedBytes payload
  }
