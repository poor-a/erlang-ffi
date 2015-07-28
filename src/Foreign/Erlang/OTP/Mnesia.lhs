> module Foreign.Erlang.OTP.Mnesia (
>   -- * Mnesia database methods.
>     backup
>   , dirtyAllKeys
>   , dirtyFirst
>   , dirtyNext
>   , dirtyLast
>   , dirtyPrev
>   , dirtyMatchObject
>   , dirtyRead
>   , dirtySelect
>   ) where

> import Foreign.Erlang.OTP.GenServer  (rpcCall)
> import Foreign.Erlang.Processes      (MBox, Node)
> import Foreign.Erlang.Types          (ErlType(..))

> mnesia                :: MBox -> Node -> String -> [ErlType] -> IO ErlType
> mnesia mbox node f as = rpcCall mbox node "mnesia" f as

> backup :: MBox -> Node -> String -> IO ErlType
> backup mbox node path = mnesia mbox node "backup" [ErlString path]

> dirtyAllKeys :: MBox -> Node -> String -> IO ErlType
> dirtyAllKeys mbox node tab = mnesia mbox node "dirty_all_keys" [ErlAtom tab]

> dirtyFirst :: MBox -> Node -> String -> IO ErlType
> dirtyFirst mbox node tab = mnesia mbox node "dirty_first" [ErlAtom tab]

> dirtyNext :: MBox -> Node -> String -> ErlType -> IO ErlType
> dirtyNext mbox node tab key = mnesia mbox node "dirty_next" [ErlAtom tab, key]

> dirtyLast :: MBox -> Node -> String -> IO ErlType
> dirtyLast mbox node tab = mnesia mbox node "dirty_last" [ErlAtom tab]

> dirtyPrev :: MBox -> Node -> String -> ErlType -> IO ErlType
> dirtyPrev mbox node tab key = mnesia mbox node "dirty_pref" [ErlAtom tab, key]

> dirtyMatchObject :: MBox -> Node -> ErlType -> IO ErlType
> dirtyMatchObject mbox node pat = mnesia mbox node "dirty_match_object" [pat]

> dirtyRead :: MBox -> Node -> String -> ErlType -> IO ErlType
> dirtyRead mbox node tab key = mnesia mbox node "dirty_read" [ErlAtom tab, key]

> dirtySelect :: MBox -> Node -> String -> ErlType -> IO ErlType
> dirtySelect mbox node tab spec = mnesia mbox node "dirty_select" [spec]
