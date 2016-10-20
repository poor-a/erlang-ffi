{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}


-- |
-- Module      : Foreign.Erlang.OTP
-- Copyright   : (c) Eric Sessoms, 2008
--               (c) Artúr Poór, 2015
-- License     : GPL3
-- 
-- Maintainer  : gombocarti@gmail.com
-- Stability   : experimental
-- Portability : portable
--

module Foreign.Erlang.Types (
  -- * Native Erlang data types
    ErlType(..)
  -- ** Conversion between native Haskell types and ErlType
  , Erlang(..)
  -- ** Easy type-safe access to tuple members
  , nth

  -- ** Internal packing functions
  , getA, getC, getErl, getN, geta, getn
  , putA, putC, putErl, putN, puta, putn
  , tag
  ) where


--import           Data.Int                      (Int64)
import           Control.Applicative
import           Control.Exception             (assert)
import           Control.Monad                 (forM, liftM)
import           Data.Binary
import           Data.Binary.Get
import           Data.Bits                     (shiftL,complement,(.|.))
import           Data.ByteString               (ByteString)
import           Data.ByteString.Lazy.Builder
import           Data.Char                     (chr, ord, isPrint,toLower,isLower,isUpper)
import           Data.Maybe                    (fromMaybe)
import           Data.Monoid                   ((<>),mconcat)
import           Data.Proxy
import           GHC.Generics
import           GHC.TypeLits
import           Prelude                hiding (id)
import qualified Data.ByteString            as Byte
import qualified Data.ByteString.Char8      as BB
import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Prelude                       (id)


nth                  :: Erlang a => Int -> ErlType -> a
nth i (ErlTuple lst) = fromErlang $ lst !! i

data ErlType = ErlNull
             | ErlInt Int
             | ErlFloat  Double
             | ErlBigInt Integer
             | ErlString String
             | ErlAtom String
             | ErlBinary [Word8]
             | ErlList [ErlType]
             | ErlTuple [ErlType]
             | ErlPid ErlType Int Int Int     -- node id serial creation
             | ErlPort ErlType Int Int        -- node id creation
             | ErlRef ErlType Int Int         -- node id creation
             | ErlNewRef ErlType Int [Word8]  -- node creation id
             deriving (Eq, Show)

{- | You can define your own instance or automatically derive it.

     The automatically derived instance works converting each constructor into
     a tuple with the same number of elements as values the constructor holds plus one, the
     first element of the tuple is an atom like the Constructor's name but converted
     into snake case, the remaining elements are the transformed values from the constructor
     in the same order. In case it is a constructor with no values, then it will be converted
     to an atom (instead of an tuple of 1 element). When checking against Constructor's names,
     some flexivility is allowed, underscores are dropped and case ignored, this way, the 
     atoms `aa_aa` and `aaa_a` will both match any constructor like `AA_aA`, `Aaaa`, `A__a_AA` 
     or similar.

     To automatically derive the class, you need to allow the language extensions  `DeriveGeneric`
     and `DeriveAnyClass`; then on the data declaration just use 
     `...deriving(..., Generic, ..., Erlang)`     
-}

class Erlang a where

    toErlang              :: a -> ErlType
    
    default toErlang      :: (Generic a,GToErlang (Rep a)) => a -> ErlType
    toErlang           = gToErlang . from


    fromErlang            :: ErlType -> a
    fromErlang x = fromMaybe (error $"error while parsing erlang type: "++show x) 
                           $ fromErlangMay x


    fromErlangMay         :: ErlType -> Maybe a
    default fromErlangMay :: (Generic a,GFromErlang (Rep a)) => ErlType -> Maybe a
    fromErlangMay         = fmap to . gFromErlang

instance Erlang ErlType where
    toErlang      = Prelude.id
    fromErlangMay = Just

instance Erlang Int where
    toErlang   x             
       | abs x <= 0x7FFFFFFF    = ErlInt x        
       | otherwise              = ErlBigInt (fromIntegral x) -- Haskell Int (might) use 64 bits whether erlang's small Int use only 32 bit

    fromErlangMay (ErlInt x)    = Just x
    fromErlangMay (ErlBigInt x) = Just (fromIntegral x)
    fromErlangMay _             = Nothing

instance Erlang Double where
    toErlang   x               = ErlFloat x 
    fromErlangMay (ErlFloat x) = Just x
    fromErlangMay _            = Nothing

instance Erlang Float where
    toErlang x                 = ErlFloat (realToFrac x)
    fromErlangMay (ErlFloat x) = Just $ realToFrac x
    fromErlangMay _            = Nothing

instance Erlang Integer where
    toErlang   x                = ErlBigInt x
    fromErlangMay (ErlInt x)    = Just$fromIntegral x
    fromErlangMay (ErlBigInt x) = Just x
    fromErlangMay _             = Nothing

instance Erlang String where
    toErlang   x             = ErlString x
    fromErlangMay ErlNull       = Just ""
    fromErlangMay (ErlString x) = Just x
    fromErlangMay (ErlAtom x)   = Just x
    fromErlangMay (ErlList xs)  = mapM (fmap chr . fromErlangMay) xs
    fromErlangMay x             = Nothing

instance Erlang Bool where
    toErlang   True                 = ErlAtom "true"
    toErlang   False                = ErlAtom "false"
    fromErlangMay (ErlAtom "true")  = Just True
    fromErlangMay (ErlAtom "false") = Just False
    fromErlangMay _                 = Nothing

instance Erlang a => Erlang [a] where
    toErlang   []              = ErlNull
    toErlang   xs              = ErlList . map toErlang $ xs
    fromErlangMay ErlNull      = Just []
    fromErlangMay (ErlList xs) = mapM fromErlangMay xs

-- Tuples are automatically instanced by generics :)

instance (Erlang a, Erlang b) => Erlang (a, b) 

instance (Erlang a, Erlang b, Erlang c) => Erlang (a, b, c)

instance (Erlang a, Erlang b, Erlang c, Erlang d) => Erlang (a, b, c, d)

instance (Erlang a, Erlang b, Erlang c, Erlang d, Erlang e) => Erlang (a, b, c, d, e) 

instance Binary ErlType where
    put = undefined
    get = getErl

      
putErl :: ErlType -> Builder
putErl (ErlInt val)
    | 0 <= val && val < 256 = tag 'a' <> putC val
    | otherwise             = tag 'b' <> putN val

putErl (ErlFloat val)       = tag 'c' <> byteString  (BB.pack . take 31 $ show val ++ repeat '\NUL')
putErl (ErlAtom val)        = tag 'd' <> putn (length val) <> putA val
putErl (ErlTuple val)
    | len < 256             = tag 'h' <> putC len <> val'
    | otherwise             = tag 'i' <> putN len <> val'
    where val' = mconcat . map putErl $ val
          len  = length val
putErl ErlNull              = tag 'j'
putErl (ErlString val)      = tag 'k' <> putn (length val) <> putA val
putErl (ErlList val)        = tag 'l' <> putN (length val) <> val' <> putErl ErlNull
    where val' = mconcat . map putErl $ val  
putErl (ErlBinary val)      = tag 'm' <> putN (length val) <> (lazyByteString . B.pack) val

putErl (ErlBigInt x) 
       | len > 255      = tag 'o' <> putN len <> byteString val 
       | otherwise      = tag 'n' <> putC len <> byteString val 
   where
     val = integerToBytes x
     len = Byte.length val -1


putErl (ErlRef node id creation) =
    tag 'e' <>
    putErl node <>
    putN id <>
    putC creation
putErl (ErlPort node id creation) = tag 'f' <> putErl node <> putN id <> putC creation
putErl (ErlPid node id serial creation) = tag 'g' <> putErl node <> putN id <> putN serial <> putC creation
putErl (ErlNewRef node creation id) =
    tag 'r' <>
    putn (length id `div` 4) <>
    putErl node <>
    putC creation <>
    (lazyByteString . B.pack) id

getErl :: Get ErlType
getErl = do
    tag <- liftM chr getC
    case tag of

      'a' -> liftM ErlInt getC

      'b' -> do x <- getN
                
                let valFrom32  
                      | x > 0x7FFFFFFF = x .|. complement 0xFFFFFFFF  
                      | otherwise      = x

                return (ErlInt valFrom32)
      'c' -> do parsed  <- reads . BB.unpack <$> getByteString 31  
                case parsed of
                  [(x,remains)]
                    | all (=='\NUL') remains -> return $ ErlFloat x 
                  _                          -> fail $ "could not parse float representation: "++show parsed

      'd' -> getn >>= liftM ErlAtom . getA
      'e' -> do
        node <- getErl
        id <- getN
        creation <- getC
        return $ ErlRef node id creation
      'f' -> do
        node <- getErl
        id <- getN
        creation <- getC
        return $ ErlPort node id creation
      'g' -> do
        node <- getErl
        id <- getN
        serial <- getN
        creation <- getC
        return $ ErlPid node id serial creation
      'h' -> getC >>= \len -> liftM ErlTuple $ forM [1..len] (const getErl)
      'i' -> getN >>= \len -> liftM ErlTuple $ forM [1..len] (const getErl)
      'j' -> return ErlNull
      'k' -> do
         len <- getn
         list <- getA len
         case all isPrint list of
           True -> return $ ErlString list
           False -> return . ErlList $ map (ErlInt . ord) list
      'l' -> do
        len <- getN
        list <- liftM ErlList $ forM [1..len] (const getErl)
        null <- getErl
        assert (null == ErlNull) $ return list
      'm' -> getN >>= liftM ErlBinary . geta

      'n' -> do  len <- getC
                 raw <- getByteString (len+1)
                 ErlBigInt <$> bytesToInteger raw
      
      'o' -> do  len <- getN
                 raw <- getByteString (len+1)
                 ErlBigInt <$> bytesToInteger raw

      'r' -> do
        len <- getn
        node <- getErl
        creation <- getC
        id <- forM [1..4*len] (const getWord8)
        return $ ErlNewRef node creation id

      x -> fail $ "Unsupported serialization code: " ++ show (ord x)


bytesToInteger :: ByteString -> Get Integer
bytesToInteger bts = case Byte.unpack bts of
                      0 : bts' -> return $          foldr step 0 bts'
                      1 : bts' -> return . negate $ foldr step 0 bts'
                      x : _    -> fail $ "Unexpected sign byte: " ++ show x
                      _        -> fail $ "Unexpected end of input at function 'bytesToInteger'"
  where
    step next acc = shiftL acc 8 + fromIntegral next


integerToBytes :: Integer -> ByteString
integerToBytes int = Byte.pack 
                   . fmap (fromIntegral.snd) 
                   . takeWhile not_zero 
                   $ iterate ((`divMod`256).fst) (abs int,sigByte)
  
  where
    not_zero (a,b)    = a + b /= 0
    sigByte | int > 0   = 0
            | otherwise = 1



tag :: Char -> Builder             
tag = charUtf8

putC :: Integral a => a -> Builder
putC = word8 . fromIntegral

putn :: Integral a => a -> Builder
putn = word16BE . fromIntegral

putN :: Integral a => a -> Builder
putN = word32BE . fromIntegral

puta :: [Word8] -> Builder
puta = lazyByteString . B.pack

putA :: String -> Builder       
putA = stringUtf8

getC :: Get Int
getC = liftM fromIntegral getWord8

getn :: Get Int
getn = liftM fromIntegral getWord16be

getN :: Get Int
getN = liftM fromIntegral getWord32be

geta :: Int -> Get [Word8]
geta = liftM B.unpack . getLazyByteString . fromIntegral

getA :: Int -> Get String
getA = liftM C.unpack . getLazyByteString . fromIntegral

-----------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------
-- Classes for generic instances
-----------------------------------------------------------------------------------------------------------------------



class GToErlang   f where

  gToErlang           :: f a -> ErlType

instance GToErlang content => GToErlang (D1 meta content) where
  gToErlang  = gToErlang . unM1


instance (GToErlang contentA,GToErlang contentB) => GToErlang (contentA :+: contentB) where
  gToErlang  (L1 x) = gToErlang  x 
  gToErlang  (R1 x) = gToErlang  x 

 

instance (Constructor meta) => GToErlang (C1 meta U1) where
  gToErlang = ErlAtom . asAtom . conName   

instance ( Constructor meta
         , Erlang      k
         ) => GToErlang (C1 meta (S1 m1 (K1 m2 k))) where
  gToErlang cons@(M1 (M1 (K1 val))) = ErlTuple [ ErlAtom . asAtom $ conName cons
                                               , toErlang val
                                               ]


instance ( Constructor    meta
         , GToTupleErlang f
         , GToTupleErlang g
         ) => GToErlang (C1 meta (f :*: g)) where
  gToErlang cons@(M1 x) = ErlTuple $ ErlAtom (asAtom$conName cons)
                                   : gToTupleErlang x


class GToTupleErlang f where
  gToTupleErlang  :: f a -> [ErlType]

instance ( GToTupleErlang f
         , GToTupleErlang g
         ) => GToTupleErlang (f :*: g)  where
  gToTupleErlang (f :*: g) = gToTupleErlang f ++ gToTupleErlang g


instance (Erlang k) => GToTupleErlang (S1 m1 (K1 m2 k))  where
  gToTupleErlang (M1 (K1 x)) = [toErlang x] 

----------------------------------------
class GFromErlang f where
  gFromErlang         :: ErlType -> Maybe (f a)


instance GFromErlang content => GFromErlang (D1 meta content) where
  gFromErlang  = fmap M1 . gFromErlang

instance (GFromErlang contentA,GFromErlang contentB) => GFromErlang (contentA :+: contentB) where
  gFromErlang x = L1 <$> gFromErlang x <|> R1<$> gFromErlang x


instance (KnownSymbol symb) => GFromErlang (C1 ('MetaCons symb a b) U1) where

  gFromErlang x = case x of
                   ErlAtom atom 
                      | asLenient (symbolVal (Proxy :: Proxy symb)) 
                                                    == asLenient atom -> Just (M1 U1)
                   _                                                  -> Nothing


instance ( KnownSymbol symb
         , GFromTupleErlang g
         , GFromTupleErlang f
         
         ) => GFromErlang (C1 ('MetaCons symb a b) ( f :*: g) )where

 gFromErlang x = case x of
                   ErlTuple (ErlAtom atom:xs)
                      | asLenient str == asLenient atom 
                      , Just (y,[]) <- gFromTupleErlang xs           -> Just (M1 y)

                   _                                                 -> Nothing
    where
      str = symbolVal (Proxy :: Proxy symb)

instance ( KnownSymbol symb
         , Erlang k
         ) => GFromErlang (C1 ('MetaCons symb a b) (S1 m1 (K1 m2 k) ) ) where

 gFromErlang x = case x of
                  ErlTuple [ErlAtom atom,y]
                   | asLenient str == asLenient atom
                   , Just val    <- fromErlangMay y   -> Just (M1 (M1 (K1 val)))

                  _                                   -> Nothing
    where
      str = symbolVal (Proxy :: Proxy symb)


class GFromTupleErlang f where
  gFromTupleErlang  :: [ErlType] -> Maybe (f a,[ErlType])

instance ( GFromTupleErlang g
         , GFromTupleErlang f
         
         ) => GFromTupleErlang  ( f :*: g)where
  
  gFromTupleErlang xs = case gFromTupleErlang xs of
                          Just (a,ys)
                             | Just (b,zs)  <- gFromTupleErlang ys   -> Just (a :*:b , zs)
                          _                                          -> Nothing

instance ( Erlang k
         ) => GFromTupleErlang (S1 m1 (K1 m2 k) ) where
  
  gFromTupleErlang xs = case xs of
                         x:ys
                           | Just val <- fromErlangMay x -> Just (M1 (K1 val),ys)
                         _                               -> Nothing



asAtom        :: String -> String
asAtom []     = []
asAtom (x:xs) = toLower x : clean xs
  where
    clean str      = case str of 
                       x:y:zs
                         | isLower x && isUpper y -> x         : '_'             : clean (y:zs)
                         | isUpper x && isUpper y -> let (bigs,other) = break isLower (zs)
                                                      in (toLower<$>x:y:bigs) ++ "_"++ clean zs 

                       xs                         -> asAtom str


asLenient     :: String -> String
asLenient     = fmap toLower .filter (/='_')



