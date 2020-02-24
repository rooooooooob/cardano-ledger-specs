{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}

module Serialization
  ( ToCBORGroup (..)
  , FromCBORGroup (..)
  , CBORGroup (..)
  , CborSeq (..)
  , CBORMap (..)
  , decodeCollection
  , decodeCollectionWithLen
  , mapHelper
--  annotated decoding
  , decodeAnnotated
  , decodeAnnotatedDecoder
  , withAnnotationSlice
  , AnnotatedDecoder (..)
  , fromCBOREmptyAnnotation
  , FromCBORAnnotatedGroup (..)
  , liftAnn
  )
where


import           Cardano.Binary (Decoder, Encoding, FromCBOR (..), ToCBOR (..), decodeBreakOr,
                     decodeListLen, decodeListLenOrIndef, decodeMapLenOrIndef, encodeBreak,
                     encodeListLen, encodeListLenIndef, encodeMapLen, encodeMapLenIndef, matchSize, DecoderError (..), Annotated (..), decodeFullDecoder, decodeWithByteSpan, decodeListWith, fromCBORMaybe)
import           Control.Monad (replicateM)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Typeable
import Data.Text (Text)

import qualified Data.ByteString.Lazy as BSL
import           Data.ByteString (ByteString)
import           Data.String (fromString)
import Codec.CBOR.Read (ByteOffset)

class Typeable a => ToCBORGroup a where
  toCBORGroup :: a -> Encoding
  listLen     :: a -> Word

newtype CBORGroup a = CBORGroup a

instance ToCBORGroup a => ToCBOR (CBORGroup a) where
  toCBOR (CBORGroup x) = encodeListLen (listLen x) <> toCBORGroup x

class Typeable a => FromCBORGroup a where
  fromCBORGroup :: Decoder s a

instance (FromCBORGroup a, ToCBORGroup a) => FromCBOR (CBORGroup a) where
  fromCBOR = do
    n <- decodeListLen
    x <- fromCBORGroup
    matchSize "CBORGroup" ((fromIntegral . toInteger . listLen) x) n
    pure $ CBORGroup x


newtype CBORMap a b = CBORMap { unwrapCBORMap :: Map a b }

instance (ToCBOR a, ToCBOR b) => ToCBOR (CBORMap a b) where
  toCBOR (CBORMap m) =
    let l = fromIntegral $ Map.size m
        contents = Map.foldMapWithKey (\k v -> toCBOR k <> toCBOR v) m
    in
    if l <= 23
    then encodeMapLen l <> contents
    else encodeMapLenIndef <> contents <> encodeBreak

instance (Ord a, FromCBOR a, FromCBOR b) => FromCBOR (CBORMap a b) where
  fromCBOR = CBORMap . Map.fromList
    <$> decodeCollection decodeMapLenOrIndef decodePair
    where
    decodePair = (,) <$> fromCBOR <*> fromCBOR

newtype CborSeq a = CborSeq { unwrapCborSeq :: Seq a }
  deriving Foldable

instance ToCBOR a => ToCBOR (CborSeq a) where
  toCBOR (CborSeq xs) =
    let l = fromIntegral $ Seq.length xs
        contents = foldMap toCBOR xs
    in
    if l <= 23
    then encodeListLen l <> contents
    else encodeListLenIndef <> contents <> encodeBreak

instance FromCBOR a => FromCBOR (CborSeq a) where
  fromCBOR = CborSeq . Seq.fromList
    <$> decodeCollection decodeListLenOrIndef fromCBOR

decodeCollection :: Decoder s (Maybe Int) -> Decoder s a -> Decoder s [a]
decodeCollection lenOrIndef el = snd <$> decodeCollectionWithLen lenOrIndef el

decodeCollectionWithLen
  :: Decoder s (Maybe Int)
  -> Decoder s a
  -> Decoder s (Int,[a])
decodeCollectionWithLen lenOrIndef el = do
  lenOrIndef >>= \case
    Just len -> (,) len <$> replicateM len el
    Nothing -> loop (0,[]) (not <$> decodeBreakOr) el
  where
  loop (n,acc) condition action = condition >>= \case
      False -> pure (n,acc)
      True -> action >>= \v -> loop (n+1, (v:acc)) condition action

mapHelper :: Decoder s b -> Decoder s [b]
mapHelper decodePart = decodeMapLenOrIndef >>= \case
  Just len -> replicateM len decodePart
  Nothing  -> loop [] (not <$> decodeBreakOr) decodePart
  where
  loop acc condition action = condition >>= \case
    False -> pure acc
    True -> action >>= \v -> loop (v:acc) condition action

--------------------------------------------------------------------------------
-- Annotated Serialization
-- todo: move this into cardano-base
--------------------------------------------------------------------------------

newtype AnnotatedDecoder s a
  = AnnotatedDecoder { unwrapAnn :: Decoder s (BSL.ByteString -> a) }
  deriving (Functor)

instance Applicative (AnnotatedDecoder s) where
  pure x = AnnotatedDecoder $ const <$> pure x
  (AnnotatedDecoder a) <*> (AnnotatedDecoder b) =
    AnnotatedDecoder $ (<*>) <$> a <*> b

decodeAnnotated :: forall a. (Typeable a, FromCBORAnnotated a)
  => BSL.ByteString
  -> Either DecoderError a
decodeAnnotated = decodeAnnotatedDecoder (fromString . show . typeRep $ Proxy @a) fromCBORAnnotated

decodeAnnotatedDecoder :: Text -> (forall s. AnnotatedDecoder s a) -> BSL.ByteString -> Either DecoderError a
decodeAnnotatedDecoder label' decoder bytes =
  (\x -> x bytes) <$> decodeFullDecoder label' (unwrapAnn decoder) bytes

withSlice :: AnnotatedDecoder s (BSL.ByteString -> a) -> AnnotatedDecoder s a
withSlice (AnnotatedDecoder dec) = AnnotatedDecoder $ do
  (k, start, end) <- decodeWithByteSpan dec
  return $ \bytes -> k bytes (sliceOffsets start end bytes)
  where
  sliceOffsets :: ByteOffset -> ByteOffset -> BSL.ByteString -> BSL.ByteString
  sliceOffsets start end = (BSL.take (end - start) . BSL.drop start)

withSlice' :: forall s a. AnnotatedDecoder s (ByteString -> a) -> AnnotatedDecoder s a
withSlice' = withSlice . fmap (. BSL.toStrict)

-- | Wrap a plain decoder into an annotated one.
liftAnn :: Decoder s a -> AnnotatedDecoder s a
liftAnn dec = AnnotatedDecoder $ const <$> dec

-- | Wrap a plain decoder into an annotated one that populates the ByteString with a slice.
withAnnotationSlice :: forall s a. Decoder s (BSL.ByteString -> a) -> AnnotatedDecoder s a
withAnnotationSlice = withSlice . liftAnn

-- | Strict version of withAnnotationSlice
withAnnotationSlice' :: forall s a. Decoder s (ByteString -> a) -> AnnotatedDecoder s a
withAnnotationSlice' = withSlice' . liftAnn

class FromCBORAnnotated a where
  fromCBORAnnotated :: forall s. AnnotatedDecoder s a

instance (FromCBOR a) => FromCBORAnnotated (Annotated a ByteString) where
  fromCBORAnnotated = withAnnotationSlice' $ Annotated <$> fromCBOR

instance FromCBORAnnotated a => FromCBORAnnotated [a] where
  fromCBORAnnotated = AnnotatedDecoder $ do
    xs <- decodeListWith (unwrapAnn fromCBORAnnotated)
    return $ \bytes -> fmap (\x -> x bytes) xs

instance FromCBORAnnotated a => FromCBORAnnotated (Maybe a) where
  fromCBORAnnotated = AnnotatedDecoder $ do
    xs <- fromCBORMaybe (unwrapAnn fromCBORAnnotated)
    return $ \bytes -> fmap (\x -> x bytes) xs

fromCBOREmptyAnnotation :: FromCBORAnnotated a => Decoder s a
fromCBOREmptyAnnotation = (\x -> x mempty) <$> unwrapAnn fromCBORAnnotated

class FromCBORAnnotatedGroup a where
  fromCBORAnnotatedGroup :: forall s. AnnotatedDecoder s a

-------------------------------------------------------------------------
-- Wrapped Decoder
-------------------------------------------------------------------------

{-
-- | Wrap both annotated and plain decoders
data WrappedDecoder a =
    Ann !(forall s. AnnotatedDecoder s a)
  | Plain !(forall s. Decoder s a)
  deriving Functor

deriving via OnlyCheckIsWHNF "WrappedDecoder" (WrappedDecoder a)
  instance NoUnexpectedThunks (WrappedDecoder a)

decodeWrapped
  :: forall a
  . (Typeable a)
  => WrappedDecoder a
  -> BSL.ByteString
  -> Either DecoderError a
decodeWrapped (Ann ad) = decodeAnnotatedDecoder (show . typeRep $ Proxy @a) ad
decodeWrapped (Plain d) = decodeFullDecoder (show . typeRep $ Proxy @a) d
-}

