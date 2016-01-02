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

module Foreign.Erlang.Network (
  -- * Low-level communication with the Erlang Port-Mapper Daemon
    epmdGetNames
  , epmdGetPort
  , epmdGetPortR4
  , register
  
  , ErlRecv
  , ErlSend
  -- ** Representation of Erlang nodes
  , Name
  , Ip
  , Node(..)
  , erlAcceptConn
  , erlConnect
  , toNetwork
  ) where

import Control.Exception        (assert, bracketOnError)
import Control.Monad            (liftM, when)
import Control.Applicative      ((<$>))
import Data.Binary.Get
import Data.Bits                ((.|.))
import Data.Char                (chr, ord)
import Data.Hash.MD5            (md5i, Str(..))
import Data.List                (unfoldr)
import Data.Word
import Network                  
import System.Directory         (getHomeDirectory)
import System.FilePath          ((</>))
import System.IO
import System.IO.Error          (ioError, userError)
import System.Random            (randomIO)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.ByteString.Lazy.Builder
import Data.Monoid ((<>),mempty)

import Foreign.Erlang.Types

erlangVersion :: Int
erlangVersion = 5

erlangProtocolVersion :: Int
erlangProtocolVersion = 131

passThrough :: Char
passThrough = 'p'

flagPublished          =  0x01
flagAtomCache          =  0x02
flagExtendedReferences =  0x04
flagDistMonitor        =  0x08
flagFunTags            =  0x10
flagDistMonitorName    =  0x20
flagHiddenAtomCache    =  0x40
flagNewFunTags         =  0x80
flagExtendedPidsPorts  = 0x100

flagExtendedReferences :: Word16
flagExtendedPidsPorts  :: Word16

flagSupportedCapability :: Word16
flagSupportedCapability = flagExtendedReferences .|. flagExtendedPidsPorts
                          
protocol :: Int
protocol = 0

highestVersion :: Int
highestVersion = 5

lowestVersion :: Int
lowestVersion = 5

getUserCookie :: IO String
getUserCookie = do
    home <- getHomeDirectory
    readFile $ home </> ".erlang.cookie"

toNetwork :: Int -> Integer -> [Word8]
toNetwork b n = reverse . take b $ unfoldr toNetwork' n ++ repeat 0
  where
    toNetwork' 0 = Nothing
    toNetwork' n = let (b, a) = n `divMod` 256 in Just (fromIntegral a, b)

erlDigest :: String -> Word32 -> [Word8]
erlDigest cookie challenge = let
    n = fromIntegral . md5i . Str $ cookie ++ show challenge
    in toNetwork 16 n

packn, packN :: Builder -> Builder
packn msg = putn (B.length msg') <> msg
    where msg' = toLazyByteString msg
packN msg = putN (B.length msg') <> msg
    where msg' = toLazyByteString msg

sendMessage :: (Builder -> Builder) -> (Builder -> IO ()) -> Builder -> IO ()
sendMessage pack out = out . pack

recvMessage :: Int -> (Int -> IO B.ByteString) -> IO B.ByteString
recvMessage hdrlen inf = (liftM (unpack hdrlen) $ inf hdrlen) >>= inf
  where
    unpack 2 = runGet getn
    unpack 4 = runGet getN

type ErlSend = (Maybe ErlType, Maybe ErlType) -> IO ()
type ErlRecv = IO (Maybe ErlType, Maybe ErlType)
      
erlSend :: (Builder -> IO ()) -> ErlSend
erlSend send (Nothing, _)    = send . lazyByteString $ B.empty
erlSend send (Just ctl, msg) = send $
    tag passThrough <>
    putMsg ctl <>
    maybe mempty putMsg msg
  where
    putMsg msg = 
      putC erlangProtocolVersion <>
      putErl msg
      
erlRecv :: IO B.ByteString -> ErlRecv
erlRecv recv = do
    bytes <- recv
    return . flip runGet bytes $ do
      empty <- isEmpty
      if empty
        then return (Nothing, Nothing)
        else do
          pt <- getC
          assert (chr pt == passThrough) $ return ()
          ctl <- getMsg
          empty <- isEmpty
          if empty
            then return (Just ctl, Nothing)
            else case ctl of
                   ErlTuple (ErlInt n:_) | n `elem` [2, 6] -> do
                     msg <- getMsg
                     return (Just ctl, Just msg)
                   _ -> return (Just ctl, Nothing)
  where
    getMsg = do
      ver <- getC
      assert (ver == erlangProtocolVersion) $ getErl

-- | Name of an Erlang node.
type Name = String

-- | Ip address of a remote Erlang node.
type Ip   = String

-- | Representation of an Erlang node on the network.     
data Node 
    = Short Name         -- ^ Local Erlang node.
    | Long Name Ip       -- ^ Remote Erlang node.
      deriving (Eq,Show)

instance Erlang Node where
    toErlang (Short name)   = ErlString name
    toErlang (Long name ip) = ErlString name
    fromErlang = undefined

erlAcceptConn :: String -> Handle -> IO (Maybe (ErlSend, ErlRecv))
erlAcceptConn self h = do
  let put = sendMessage packn (hPutBuilder h)
      get = recvMessage 2 (B.hGet h)
  result <- handshakeAccept put get self
  case result of
    Just name -> do
      let put' = sendMessage packN (hPutBuilder h)
          get' = recvMessage 4 (B.hGet h)
      return . Just $ (erlSend put', erlRecv get')
    Nothing -> return Nothing

handshakeAccept :: (Builder -> IO ()) -> IO B.ByteString -> String -> IO (Maybe String)
handshakeAccept put get self = do
  name <- recvName
  sendStatus
  myChallenge <- randomIO :: IO Int
  cookie <- getUserCookie
  let myDigest = erlDigest cookie (fromIntegral myChallenge)
  sendChallenge myChallenge
  (challenge, digest) <- recvChallengeReply
  if digest == myDigest
    then do
      sendChallengeAck cookie (fromIntegral challenge)
      return . Just $ name
    else 
        return Nothing
    where
      recvName = do
        msg <- get
        return $ flip runGet msg $ do
          _t <- getTag
          _version <- getn
          _flags <- getN
          name <- getStrRem
          return name

      sendStatus = put $ tag 's' <> putA "ok"

      sendChallenge challenge = do
        put $ tag 'n' <>
              putn erlangVersion <>
              putN flagSupportedCapability <>
              putN challenge <>
              putA self

      recvChallengeReply = do
        reply <- get
        return $ flip runGet reply $ do
          _t <- getTag
          challenge <- getN
          digest <- getW8Rem
          return (challenge,digest)

      sendChallengeAck cookie challenge = put $
        tag 'a' <>
        puta (erlDigest cookie challenge)
         
erlConnect :: String -> Node -> IO (ErlSend, ErlRecv)
erlConnect self node = withSocketsDo $ do
    port <- epmdGetPort node
    let port' = PortNumber . fromIntegral $ port
    withNode epmd port' $ \h -> do
        let out = sendMessage packn (hPutBuilder h)
        let inf = recvMessage 2 (B.hGet h)
        handshake out inf self
        let out' = sendMessage packN (hPutBuilder h)
        let inf' = recvMessage 4 (B.hGet h)
        return (erlSend out', erlRecv inf')
    where epmd = case node of
                   Short _    -> epmdLocal
                   Long  _ ip -> ip

                     
handshake :: (Builder -> IO ()) -> IO B.ByteString -> String -> IO ()
handshake out inf self = do
    cookie <- getUserCookie
    sendName
    recvStatus
    challenge <- recvChallenge
    let reply = erlDigest cookie challenge
    challenge' <- liftM fromIntegral (randomIO :: IO Int)
    challengeReply reply challenge'
    recvChallengeAck cookie challenge'

  where
    sendName = out $
        tag 'n' <>
        putn erlangVersion <>
        putN flagSupportedCapability <>
        putA self

    recvStatus = do
        msg <- inf
        assert ("sok" == B.unpack msg) $ return ()

    recvChallenge = do
        msg <- inf
        return . flip runGet msg $ do
            _tag <- getC
            _version <- getn 
            _flags <- getN
            challenge <- getWord32be
            return challenge

    challengeReply reply challenge = out $
        tag 'r' <>
        word32BE challenge <>
        puta reply

    recvChallengeAck cookie challenge = do
        let digest = erlDigest cookie challenge
        msg <- inf
        let reply = take 16 . tail . map (fromIntegral . ord) . B.unpack $ msg
        assert (digest == reply) $ return ()

epmdLocal :: HostName
epmdLocal = "127.0.0.1"
            
epmdPort :: PortID
--epmdPort = Service "epmd"
epmdPort = PortNumber 4369

withNode :: String -> PortID -> (Handle -> IO a) -> IO a
withNode epmd port = withSocketsDo . bracketOnError
    (connectTo epmd port)
    hClose

register :: String -> Int -> IO Handle
register name port = do
  h <- connectTo epmdLocal epmdPort
  sendMessage packn (hPutBuilder h) alive2Req
  reply <- B.hGetContents h
  let result_ = flip runGet reply $ do
                  _tag <- getC
                  result <- getC
                  _creation <- getn
                  return result
  if result_ == 0
    then return h
    else ioError (userError $ "epmd returned error code " ++ show result_)
          where
      alive2Req = tag 'x' <>
                  putn port <>
                  putn protocol <>
                  putn highestVersion <>
                  putn lowestVersion <>
                  (putn . length) name <>
                  putA name <>
                  putn extraBytes

      extraBytes :: Int
      extraBytes = 0

withEpmd :: String -> (Handle -> IO a) -> IO a
withEpmd epmd = withSocketsDo . bracketOnError
    (connectTo epmd epmdPort)
    hClose

epmdSend :: String -> String -> IO B.ByteString
epmdSend epmd msg = withEpmd epmd $ \hdl -> do
    let out = putn (length msg) <> putA msg
    hPutBuilder hdl out
    hFlush hdl
    B.hGetContents hdl

-- | Return the names and addresses of registered local Erlang nodes.
epmdGetNames :: IO [String]
epmdGetNames = do
    reply <- epmdSend epmdLocal "n"
    let txt = runGet (getN >> liftM B.unpack getRemainingLazyByteString) reply
    return . lines $ txt

-- | Return the port address of a named Erlang node.
epmdGetPort :: Node -> IO Int
epmdGetPort node = do
  reply <- epmdSend epmd $ 'z' : nodeName
  return $ flip runGet reply $ do
                     _ <- getC
                     res <- getC
                     if res == 0
                       then getn
                       else error $ "epmdGetPort: node not found: " ++ show node
    where (nodeName, epmd) = case node of
                           Short name    -> (name, epmdLocal)
                           Long  name ip -> (name, ip)

-- | Returns (port, nodeType, protocol, vsnMax, vsnMin, name, extra)
epmdGetPortR4 :: String -> String -> IO (Int, Int, Int, Int, Int, String, String)
epmdGetPortR4 epmd name = do
    reply <- epmdSend epmd $ 'z' : name
    return $ flip runGet reply $ do
        _ <- getn
        port <- getn
        nodeType <- getC
        protocol_ <- getC
        vsnMax <- getn
        vsnMin <- getn
        name_ <- getn >>= getA
        extra <- liftM B.unpack getRemainingLazyByteString
        return (port, nodeType, protocol_, vsnMax, vsnMin, name_, extra)
