> module Foreign.Erlang.Processes (
>   -- * Low-level communication.
>   -- ** Represents a Haskell node (program).
>     Self
>   , createSelf
>   -- ** Represents a Haskell process (thread).
>   , MBox
>   , createMBox
>   , mboxRef
>   , mboxSelf
>   -- ** Represents Erlang nodes and processes.
>   , Node
>   , Pid
>   -- ** Communication to/from Erlang.
>   , mboxRecv
>   , mboxRecv'
>   , mboxSend
>   ) where

> import Control.Concurrent  (forkIO)
> import Control.Concurrent.MVar
> import Data.Maybe          (fromJust)
> import Foreign.Erlang.Network
> import Foreign.Erlang.Types

> -- | The name of an Erlang node on the network.
> type Node = String

> data ErlMessage = ErlRegister (ErlType -> IO ())
>                 | ErlGenRef ErlType
>                 | ErlSend String ErlType ErlType
>                 | ErlRegSend ErlType String String ErlType
>                 | ErlLink ErlType String ErlType
>                 | ErlUnlink ErlType String ErlType
>                 | ErlExit ErlType String ErlType ErlType
>                 | ErlExit2 ErlType String ErlType ErlType
>                 | ErlDispatch ErlType ErlType
>                 | ErlStop
>                 deriving Show

> instance Show (a -> b) where
>     show _ = "<function>"

> -- | Represents a Haskell node.  There should be one of these per process.
> data Self = Self { send :: ErlMessage -> IO () }

> genPid nodename id = ErlPid (ErlAtom nodename) id 0 1
> genRef nodename id = ErlNewRef (ErlAtom nodename) 1 . toNetwork 4 . fromIntegral $ id

> -- | Instantiate a Haskell node.  This initializes the FFI.
> createSelf          :: String -> IO Self
> createSelf nodename = do
>     inbox <- newEmptyMVar
>     forkIO $ self nodename inbox
>     return . Self $ putMVar inbox

> self                :: String -> MVar ErlMessage -> IO ()
> self nodename inbox = loop 1 [] []
>   where
>     loop id mboxes nodes = do
>         msg <- takeMVar inbox
>         case msg of
>           ErlRegister mbox -> do
>             let pid = genPid nodename id
>             mbox pid
>             loop (id+1) ((pid, mbox) : mboxes) nodes
>           ErlGenRef pid -> do
>             let ref = genRef nodename id
>             maybe (return ()) ($ ref) $ lookup pid mboxes
>             loop (id+1) mboxes nodes
>           ErlSend node pid msg -> do
>             let ctl = toErlang (ErlInt 2, ErlAtom "", pid)
>             (mnode, nodes') <- findNode node nodes
>             case mnode of
>               Just n  -> n (Just ctl, Just msg)
>               Nothing -> return ()
>             loop id mboxes nodes'
>           ErlRegSend from node pid msg -> do
>             let ctl = toErlang (ErlInt 6, from, ErlAtom "", ErlAtom pid)
>             (mnode, nodes') <- findNode node nodes
>             case mnode of
>               Just n  -> n (Just ctl, Just msg)
>               Nothing -> return ()
>             loop id mboxes nodes'
>           ErlLink from to pid -> do
>               let ctl = toErlang (ErlInt 1, from, pid)
>               (node, nodes') <- findNode to nodes
>               fromJust node (Just ctl, Nothing)
>               loop id mboxes nodes'
>           ErlUnlink from to pid -> do
>               let ctl = toErlang (ErlInt 4, from, pid)
>               (node, nodes') <- findNode to nodes
>               fromJust node (Just ctl, Nothing)
>               loop id mboxes nodes'
>           ErlExit from to pid reason -> do
>               let ctl = toErlang (ErlInt 3, from, to, reason)
>               (node, nodes') <- findNode to nodes
>               fromJust node (Just ctl, Nothing)
>               loop id mboxes nodes'
>           ErlExit2 from to pid reason -> do
>               let ctl = toErlang (ErlInt 8, from, to, reason)
>               (node, nodes') <- findNode to nodes
>               fromJust node (Just ctl, Nothing)
>               loop id mboxes nodes'
>           ErlDispatch ctl msg -> do
>             case ctl of
>               ErlTuple [ErlInt 2, _, pid] ->
>                 maybe (return ()) ($ msg) $ lookup pid mboxes
>               _ -> return ()
>             loop id mboxes nodes
>           ErlStop -> return ()

>     findNode to nodes =
>         case lookup to nodes of
>           Just node -> return (Just node, nodes)
>           Nothing   -> do
>             (send, recv) <- erlConnect nodename to
>             mvar <- newEmptyMVar
>             forkIO $ nodeSend mvar send
>             forkIO $ nodeRecv mvar recv inbox
>             let node = putMVar mvar
>             return (Just node, ((to, node) : nodes))

A `nodeSend` thread is responsible for communication to an Erlang
process.  It receives messages in an `MVar` and forwards them across
the network.

> nodeSend mvar send = loop
>   where
>     loop = takeMVar mvar >>= send >> loop

A `nodeRecv` thread is responsible for communication from an Erlang
process.  It receives messages from the network and dispatches them as
appropriate.

> nodeRecv mvar recv outbox = loop
>   where
>     loop = do
>         (mctl, mmsg) <- recv
>         case mctl of
>             -- Nothing is a keepalive.  All we want to do is echo it.
>             Nothing  -> putMVar mvar (Nothing, Nothing)
>             -- A real message goes to self to be dispatched.
>             Just ctl -> putMVar outbox $ ErlDispatch ctl (fromJust mmsg)
>         loop

> -- | Haskell threads don't natively have Erlang process IDs.  Instead, we
> -- use a mailbox abstraction that we can assign PIDs to for communication
> -- with Erlang.

> data MBox = MBox ErlType (MVar ErlType) Self

> -- | Represents a foreign (Erlang) process.  A process can be identified
> -- either by its low-level ID (Left pid) or by its registered name (Right name).
  
> type Pid  = Either ErlType String

> -- | Return the PID of the given mailbox.

> mboxSelf                :: MBox -> ErlType
> mboxSelf (MBox pid _ _) = pid

> -- | Return a new unique object reference.

> mboxRef                        :: MBox -> IO ErlType
> mboxRef mbox@(MBox pid _ self) = send self (ErlGenRef pid) >> mboxRecv mbox

Send an arbitrary message to the specified node and process.

> -- | {Node, Pid} ! Msg.
> mboxSend :: Erlang a => MBox -> Node -> Pid -> a -> IO ()
> mboxSend (MBox _    _ self) node (Left  pid) msg = send self $ ErlSend node pid (toErlang msg)
> mboxSend (MBox from _ self) node (Right pid) msg = send self $ ErlRegSend from node pid (toErlang msg)

> -- | Receive the next message addressed to this mailbox.

> mboxRecv                  :: MBox -> IO ErlType
> mboxRecv (MBox _ inbox _) = takeMVar inbox

> -- | Receive a reply message.  That is, looks for the next message
> -- identified by the given reference.
  
> mboxRecv'          :: MBox -> ErlType -> IO ErlType
> mboxRecv' mbox ref = do
>     msg <- mboxRecv mbox
>     case msg of
>         ErlTuple [ref', result] | ref' == ref -> return result
>         _                                     -> mboxRecv' mbox ref

> -- | Create a new process on the Haskell side.  Usually corresponds
> -- to a thread but doesn't need to.
          
> createMBox      :: Self -> IO MBox
> createMBox self = do
>     inbox <- newEmptyMVar
>     send self $ ErlRegister (putMVar inbox)
>     pid <- takeMVar inbox
>     return $ MBox pid inbox self
