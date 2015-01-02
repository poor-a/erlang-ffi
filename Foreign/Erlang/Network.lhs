> module Foreign.Erlang.Network (
>   -- * Low-level communication with the Erlang Port-Mapper Daemon.
>     epmdGetNames
>   , epmdGetPort
>   , epmdGetPortR4
>   
>   , ErlRecv
>   , ErlSend
>   , erlConnect
>   , toNetwork
>   ) where

> import Control.Exception        (assert, bracketOnError)
> import Control.Monad            (liftM)
> import Data.Binary              (decode, encode)
> import Data.Binary.Get
> import Data.Binary.Put
> import Data.Bits                ((.|.))
> import Data.Char                (chr, ord)
> import Data.Digest.OpenSSL.MD5  (md5sum)
> import Data.List                (unfoldr)
> import Data.Word
> import Foreign.Erlang.Types
> import Network                  (PortID(..), connectTo, withSocketsDo)
> import Network.Socket           (PortNumber(..))
> import Numeric                  (readHex)
> import System.Directory         (getHomeDirectory)
> import System.FilePath          ((</>))
> import System.IO
> import System.IO.Unsafe         (unsafePerformIO)
> import System.Random            (randomIO)
> import qualified Data.ByteString.Char8      as C
> import qualified Data.ByteString.Lazy.Char8 as B

> erlangVersion = 5
> erlangProtocolVersion = 131
> passThrough = 'p'

> flagPublished          =  0x01
> flagAtomCache          =  0x02
> flagExtendedReferences =  0x04
> flagDistMonitor        =  0x08
> flagFunTags            =  0x10
> flagDistMonitorName    =  0x20
> flagHiddenAtomCache    =  0x40
> flagNewFunTags         =  0x80
> flagExtendedPidsPorts  = 0x100

> getUserCookie = do
>     home <- getHomeDirectory
>     readFile $ home </> ".erlang.cookie"

> toNetwork     :: Int -> Integer -> [Word8]
> toNetwork b n = reverse . take b $ unfoldr toNetwork' n ++ repeat 0
>   where
>     toNetwork' 0 = Nothing
>     toNetwork' n = let (b, a) = n `divMod` 256 in Just (fromIntegral a, b)

> ntohs n = let (b, a) = n `divMod` 256 in 256*a + b

> erlDigest                  :: String -> Word32 -> [Word8]
> erlDigest cookie challenge = let
>     [(n, _)] = readHex . md5sum . C.pack $ cookie ++ show challenge
>     in toNetwork 16 n

> packn, packN :: B.ByteString -> Put
> packn msg = putn (fromIntegral . B.length $ msg) >> putLazyByteString msg
> packN msg = putN (fromIntegral . B.length $ msg) >> putLazyByteString msg

> sendMessage :: (B.ByteString -> Put) -> (B.ByteString -> IO ()) -> B.ByteString -> IO ()
> sendMessage pack out = out . runPut . pack

> recvMessage            :: Int -> (Int -> IO B.ByteString) -> IO B.ByteString
> recvMessage hdrlen inf = (liftM (unpack hdrlen) $ inf hdrlen) >>= inf
>   where
>     unpack 2 = runGet getn
>     unpack 4 = runGet getN

> type ErlSend = (Maybe ErlType, Maybe ErlType) -> IO ()
> type ErlRecv = IO (Maybe ErlType, Maybe ErlType)
      
> erlSend :: (B.ByteString -> IO ()) -> ErlSend
> erlSend send (Nothing, _)    = send B.empty
> erlSend send (Just ctl, msg) = send . runPut $ do
>     tag passThrough
>     putMsg ctl
>     maybe (return ()) putMsg msg
>   where
>     putMsg msg = do
>       putC erlangProtocolVersion
>       putErl msg
      
> erlRecv      :: IO B.ByteString -> ErlRecv
> erlRecv recv = do
>     bytes <- recv
>     return . flip runGet bytes $ do
>       empty <- isEmpty
>       if empty
>         then return (Nothing, Nothing)
>         else do
>           pt <- getC
>           assert (chr pt == passThrough) $ return ()
>           ctl <- getMsg
>           empty <- isEmpty
>           if empty
>             then return (Just ctl, Nothing)
>             else case ctl of
>                    ErlTuple (ErlInt n:_) | n `elem` [2, 6] -> do
>                      msg <- getMsg
>                      return (Just ctl, Just msg)
>                    _ -> return (Just ctl, Nothing)
>   where
>     getMsg = do
>       ver <- getC
>       assert (ver == erlangProtocolVersion) $ getErl

> erlConnect           :: String -> String -> IO (ErlSend, ErlRecv)
> erlConnect self node = withSocketsDo $ do
>     port <- epmdGetPort node
>     let port' = PortNumber (PortNum . fromIntegral . ntohs $ port)
>     bracketOnError
>       (connectTo epmdHost port' >>= \h -> hSetBuffering h NoBuffering >> return h)
>       hClose $ \h -> do
>         let out = sendMessage packn (B.hPut h)
>         let inf = recvMessage 2 (B.hGet h)
>         handshake out inf self
>         let out' = sendMessage packN (\s -> B.hPut h s >> hFlush h)
>         let inf' = recvMessage 4 (B.hGet h)
>         return (erlSend out', erlRecv inf')

> handshake              :: (B.ByteString -> IO ()) -> IO B.ByteString -> String -> IO ()
> handshake out inf self = do
>     cookie <- getUserCookie
>     sendName
>     recvStatus
>     challenge <- recvChallenge
>     let reply = erlDigest cookie challenge
>     challenge' <- liftM fromIntegral (randomIO :: IO Int)
>     challengeReply reply challenge'
>     recvChallengeAck cookie challenge'

>   where
>     sendName = out . runPut $ do
>         tag 'n'
>         putn erlangVersion
>         putN $ flagExtendedReferences .|. flagExtendedPidsPorts
>         putA self

>     recvStatus = do
>         msg <- inf
>         assert ("sok" == B.unpack msg) $ return ()

>     recvChallenge = do
>         msg <- inf
>         return . flip runGet msg $ do
>             tag <- getC
>             version <- getn
>             flags <- getN
>             challenge <- getWord32be
>             return challenge

>     challengeReply reply challenge = out . runPut $ do
>         tag 'r'
>         putWord32be challenge
>         puta reply

>     recvChallengeAck cookie challenge = do
>         let digest = erlDigest cookie challenge
>         msg <- inf
>         let reply = take 16 . drop 1 . map (fromIntegral . ord) . B.unpack $ msg
>         assert (digest == reply) $ return ()

> epmdHost = "127.0.0.1"
> epmdPort = Service "epmd"

> withEpmd = withSocketsDo . bracketOnError
>     (connectTo epmdHost epmdPort >>= \h -> hSetBuffering h NoBuffering >> return h)
>     hClose

> epmdSend     :: String -> IO B.ByteString
> epmdSend msg = withEpmd $ \hdl -> do
>     let out = runPut $ putn (length msg) >> putA msg
>     B.hPut hdl out
>     hFlush hdl
>     B.hGetContents hdl

> -- | Return the names and addresses of all registered Erlang nodes.
> epmdGetNames :: IO [String]
> epmdGetNames = do
>     reply <- epmdSend "n"
>     let txt = runGet (getN >> liftM B.unpack getRemainingLazyByteString) reply
>     return . lines $ txt

> -- | Return the port address of a named Erlang node.
> epmdGetPort      :: String -> IO Int
> epmdGetPort name = do
>     reply <- epmdSend $ 'p' : name
>     return $ runGet getn reply

> -- | Returns (port, nodeType, protocol, vsnMax, vsnMin, name, extra)
> epmdGetPortR4      :: String -> IO (Int, Int, Int, Int, Int, String, String)
> epmdGetPortR4 name = do
>     reply <- epmdSend $ 'z' : name
>     return $ flip runGet reply $ do
>         getn
>         port <- getn
>         nodeType <- getC
>         protocol <- getC
>         vsnMax <- getn
>         vsnMin <- getn
>         name <- getn >>= getA
>         extra <- liftM B.unpack getRemainingLazyByteString
>         return (port, nodeType, protocol, vsnMax, vsnMin, name, extra)
