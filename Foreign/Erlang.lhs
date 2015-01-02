> -----------------------------------------------------------------------------
> -- |
> -- Module      : Foreign.Erlang
> -- Copyright   : Eric Sessoms
> -- License     : GPL3 (see COPYING)
> -- 
> -- Maintainer  : Eric Sessoms <nubgames@gmail.com>
> -- Stability   : alpha
> -- Portability : portable
> --
> -- Speaks the Erlang network protocol and impersonates an Erlang node
> -- on the network.  Fully capable of bi-directional communication with
> -- Erlang.  Erlang types are, as far as reasonable, mapped to Haskell
> -- types.  Messages to Erlang are just function calls in Haskell, and
> -- messages from Erlang are delivered to MVars.
> --
> -----------------------------------------------------------------------------

> module Foreign.Erlang (
>     module Foreign.Erlang.Network
>   , module Foreign.Erlang.OTP
>   , module Foreign.Erlang.Processes
>   , module Foreign.Erlang.Types
>   , module Foreign.Erlang.Utilities
>   ) where

> import Foreign.Erlang.Network hiding (toNetwork)
> import Foreign.Erlang.OTP
> import Foreign.Erlang.Processes
> import Foreign.Erlang.Utilities
>     
> import Foreign.Erlang.Types hiding (
>     getA, getC, getErl, getN, geta, getn
>   , putA, putC, putErl, putN, puta, putn
>   , tag
>   )
