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

module Foreign.Erlang.Processes (
  -- * Low-level communication
  -- ** Representation of a Haskell node (program)
    Self
  , createSelf
  -- ** Representation of a Haskell process (thread)
  , MBox
  , createMBox
  , mboxRef
  , mboxSelf
  -- ** Representation of Erlang nodes and processes
  , Pid
  -- ** Communication to and from Erlang
  , mboxRecv
  , mboxRecv'
  , mboxSend
  ) where

import Prelude hiding (id)
import Control.Concurrent  (forkIO)
import Control.Concurrent.MVar
import Data.Maybe          (fromJust)
import System.IO (hClose)
import Network                  

import Foreign.Erlang.Network
import Foreign.Erlang.Types

data ErlMessage = ErlRegister (ErlType -> IO ())
                | ErlAcceptNode Node ErlSend ErlRecv
                | ErlGenRef ErlType
                | ErlSend Node ErlType ErlType
                | ErlRegSend ErlType Node String ErlType
                | ErlLink ErlType Node ErlType
                | ErlUnlink ErlType Node ErlType
                | ErlExit ErlType Node ErlType ErlType
                | ErlExit2 ErlType Node ErlType ErlType
                | ErlDispatch ErlType ErlType
                | ErlStop

-- | Represents a Haskell node.  There should be one of these per process.
data Self = Self { send :: ErlMessage -> IO () }

genPid :: String -> Int -> ErlType
genPid nodename id = ErlPid (ErlAtom nodename) id 0 1

genRef :: String -> Int -> ErlType
genRef nodename id = ErlNewRef (ErlAtom nodename) 1 . toNetwork 4 . fromIntegral $ id

-- | Instantiate a Haskell node.  This initializes the FFI.
createSelf :: String -> Int -> IO Self
createSelf nodename port = do
  inbox <- newEmptyMVar
  _ <- forkIO $ self nodename port inbox
  return . Self $ putMVar inbox

self :: String -> Int -> MVar ErlMessage -> IO ()
self fullname port inbox = do
  _ <- forkIO $ listen fullname port (Self $ putMVar inbox)
  let nodename = takeWhile (/= '@') fullname
  epdm <- register nodename port
  loop 1 [] [] epdm
  where
    loop id mboxes nodes epmd = do
        msg <- takeMVar inbox
        case msg of
          ErlRegister mbox -> do
            let pid = genPid fullname id
            mbox pid
            loop (id+1) ((pid, mbox) : mboxes) nodes epmd
          ErlAcceptNode node send recv -> do
            outbox <- newEmptyMVar
            _ <- forkIO $ nodeSend outbox send
            _ <- forkIO $ nodeRecv outbox recv inbox
            let deliver = putMVar outbox
            loop id mboxes ((node,deliver):nodes) epmd
          ErlGenRef pid -> do
            let ref = genRef fullname id
            maybe (return ()) ($ ref) $ lookup pid mboxes
            loop (id+1) mboxes nodes epmd
          ErlSend node pid msg -> do
            let ctl = toErlang (ErlInt 2, ErlAtom "", pid)
            (mnode, nodes') <- findNode node nodes
            case mnode of
              Just n  -> n (Just ctl, Just msg)
              Nothing -> return ()
            loop id mboxes nodes' epmd
          ErlRegSend from node pid msg -> do
            let ctl = toErlang (ErlInt 6, from, ErlAtom "", ErlAtom pid)
            (mnode, nodes') <- findNode node nodes
            case mnode of
              Just n  -> n (Just ctl, Just msg)
              Nothing -> return ()
            loop id mboxes nodes' epmd
          ErlLink from to pid -> do
              let ctl = toErlang (ErlInt 1, from, pid)
              (node, nodes') <- findNode to nodes
              fromJust node (Just ctl, Nothing)
              loop id mboxes nodes' epmd
          ErlUnlink from to pid -> do
              let ctl = toErlang (ErlInt 4, from, pid)
              (node, nodes') <- findNode to nodes
              fromJust node (Just ctl, Nothing)
              loop id mboxes nodes' epmd
          ErlExit from to pid reason -> do
              let ctl = toErlang (ErlInt 3, from, to, reason)
              (node, nodes') <- findNode to nodes
              fromJust node (Just ctl, Nothing)
              loop id mboxes nodes' epmd
          ErlExit2 from to pid reason -> do
              let ctl = toErlang (ErlInt 8, from, to, reason)
              (node, nodes') <- findNode to nodes
              fromJust node (Just ctl, Nothing)
              loop id mboxes nodes' epmd
          ErlDispatch ctl msg -> do
            case ctl of
              ErlTuple [ErlInt 2, _, pid] ->
                maybe (return ()) ($ msg) $ lookup pid mboxes
              _ -> return ()
            loop id mboxes nodes epmd
          ErlStop -> hClose epmd

    findNode to nodes =
        case lookup to nodes of
          Just node -> return (Just node, nodes)
          Nothing   -> do
            (send, recv) <- erlConnect fullname to
            mvar <- newEmptyMVar
            _ <- forkIO $ nodeSend mvar send
            _ <- forkIO $ nodeRecv mvar recv inbox
            let node = putMVar mvar
            return (Just node, ((to, node) : nodes))

listen :: String -> Int -> Self -> IO ()
listen selfName port self = do
  socket <- listenOn (PortNumber . fromIntegral $ port)
  loop socket
       where
         loop socket = do
           (h,_,_) <- accept socket
           _ <- forkIO $ do
             result <- erlAcceptConn selfName h
             maybe (return ()) (\(node,sendnode,recvnode) -> send self $ ErlAcceptNode node sendnode recvnode) result
           loop socket

-- | A `nodeSend` thread is responsible for communication to an Erlang
-- process.  It receives messages in an `MVar` and forwards them across
-- the network.

nodeSend mvar send = loop
  where
    loop = takeMVar mvar >>= send >> loop

-- | A `nodeRecv` thread is responsible for communication from an Erlang
-- process.  It receives messages from the network and dispatches them as
-- appropriate.

nodeRecv mvar recv outbox = loop
  where
    loop = do
        (mctl, mmsg) <- recv
        case mctl of
            -- Nothing is a keepalive.  All we want to do is echo it.
            Nothing  -> putMVar mvar (Nothing, Nothing)
            -- A real message goes to self to be dispatched.
            Just ctl -> putMVar outbox $ ErlDispatch ctl (fromJust mmsg)
        loop

-- | Haskell threads don't natively have Erlang process IDs.  Instead, we
-- use a mailbox abstraction that we can assign PIDs to for communication
-- with Erlang.

data MBox = MBox ErlType (MVar ErlType) Self

-- | Represents a foreign (Erlang) process.  A process can be identified
-- either by its low-level ID (Left pid) or by its registered name (Right name).
  
type Pid  = Either ErlType String

-- | Return the PID of the given mailbox.

mboxSelf                :: MBox -> ErlType
mboxSelf (MBox pid _ _) = pid

-- | Return a new unique object reference.

mboxRef                        :: MBox -> IO ErlType
mboxRef mbox@(MBox pid _ self) = send self (ErlGenRef pid) >> mboxRecv mbox

-- | Send an arbitrary message to the specified node and process. In Erlang equivalent to
--
-- > {Node, Pid} ! Msg.

mboxSend :: Erlang a => MBox -> Node -> Pid -> a -> IO ()
mboxSend (MBox _    _ self) node (Left  pid) msg = send self $ ErlSend node pid (toErlang msg)
mboxSend (MBox from _ self) node (Right pid) msg = send self $ ErlRegSend from node pid (toErlang msg)

-- | Receive the next message addressed to this mailbox.

mboxRecv                  :: MBox -> IO ErlType
mboxRecv (MBox _ inbox _) = takeMVar inbox

-- | Receive a reply message.  That is, looks for the next message
-- identified by the given reference.
  
mboxRecv'          :: MBox -> ErlType -> IO ErlType
mboxRecv' mbox ref = do
    msg <- mboxRecv mbox
    case msg of
        ErlTuple [ref', result] | ref' == ref -> return result
        _                                     -> mboxRecv' mbox ref

-- | Create a new process on the Haskell side.  Usually corresponds
-- to a thread but doesn't need to.
          
createMBox      :: Self -> IO MBox
createMBox self = do
    inbox <- newEmptyMVar
    send self $ ErlRegister (putMVar inbox)
    pid <- takeMVar inbox
    return $ MBox pid inbox self
