-----------------------------------------------------------------------------
-- |
-- Module      : Foreign.Erlang.OTP.GenServer
-- Copyright   : (c) Eric Sessoms, 2008
--               (c) Artúr Poór, 2015
-- License     : GPL3 (see COPYING)
-- 
-- Maintainer  : Eric Sessoms <nubgames@gmail.com>
-- Stability   : alpha
-- Portability : portable
--
-----------------------------------------------------------------------------

module Foreign.Erlang.OTP.GenServer (
  -- * High-level communication.
    genCall
  , genCast
  , rpcCall
  , rpcCast
  ) where

import Foreign.Erlang.Types      (Erlang, ErlType(..))
import Foreign.Erlang.Processes

-- | gen_server:cast(Pid, Msg)

genCast :: Erlang a => MBox -> Node -> Pid -> a -> IO ()
genCast mbox node pid msg = mboxSend mbox node pid (ErlAtom "$gen_cast", msg)

-- | gen_server:call(Pid, Msg)

genCall :: Erlang a => MBox -> Node -> Pid -> a -> IO ErlType
genCall mbox node pid msg = do
    ref <- mboxRef mbox
    mboxSend mbox node pid (ErlAtom "$gen_call", (mboxSelf mbox, ref), msg)
    mboxRecv' mbox ref

-- | rpc:cast(Node, Module, Function, Arguments)

rpcCast :: MBox -> Node -> String -> String -> [ErlType] -> IO ()
rpcCast mbox node m f as = genCast mbox node (Right "rex") $
    (ErlAtom "cast", ErlAtom m, ErlAtom f, as, mboxSelf mbox)

-- | rpc:call(Node, Module, Function, Arguments)

rpcCall :: MBox -> Node -> String -> String -> [ErlType] -> IO ErlType
rpcCall mbox node m f as = genCall mbox node (Right "rex") $
    (ErlAtom "call", ErlAtom m, ErlAtom f, as, mboxSelf mbox)
