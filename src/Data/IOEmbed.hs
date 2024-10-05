{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}

module Data.IOEmbed (embedIO, embedIOLit) where

import Language.Haskell.TH
import Data.Typeable (Typeable, Proxy (..), typeRep)
import Data.Word (Word8)
import Language.Haskell.TH.Syntax (Bytes)
import Unsafe.Coerce (unsafeCoerce)

embedIO :: forall a. Typeable a => IO a -> Q Exp
embedIO = embedIOLit . fmap toLit

embedIOLit :: IO Lit -> Q Exp
embedIOLit io = do
  litResult <- runIO io
  return $ LitE litResult

toLit :: forall a. Typeable a => a -> Lit
toLit x
  | typeRep' == typeRep (Proxy :: Proxy Char)     = CharL $ unsafeCoerce x -- unsafeCoerce is safe* here since we type-checked manually.
  | typeRep' == typeRep (Proxy :: Proxy String)   = StringL $ unsafeCoerce x
  | typeRep' == typeRep (Proxy :: Proxy Integer)  = IntegerL $ unsafeCoerce x
  | typeRep' == typeRep (Proxy :: Proxy Rational) = RationalL $ unsafeCoerce x
  | typeRep' == typeRep (Proxy :: Proxy [Word8])  = StringPrimL $ unsafeCoerce x
  | typeRep' == typeRep (Proxy :: Proxy Bytes)    = BytesPrimL $ unsafeCoerce x
  | otherwise = error $ "Type " <> show typeRep' <> " is not embeddable"
                <> "\nEmbeddable types are: Char, String, Integer, Rational, [Word8], Bytes"
  where typeRep' = typeRep (Proxy :: Proxy a)
