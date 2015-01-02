> module Foreign.Erlang.Utilities (
>   -- * Miscellaneous utilities.
>     erlangTimeToSeconds
>   , secondsToErlangTime
>   ) where

> import Foreign.Erlang.Types  (ErlType(..))

Erlang's native time format is a tuple: {MegaSeconds, Seconds,
MicroSeconds}.  Luckily though, Erlang uses the same epoch, so no
further conversion is necessary.

> -- | Convert a tuple (from erlang:now()) to seconds from Jan 1, 1970.

> erlangTimeToSeconds :: Integral a => ErlType -> a
> erlangTimeToSeconds (ErlTuple [ErlInt ms, ErlInt s, _]) =
>     fromIntegral $ 1000000*ms + s

> -- | Convert seconds to an Erlang tuple representing time.

> secondsToErlangTime :: Integral a => a -> ErlType
> secondsToErlangTime sec = ErlTuple [ErlInt ms, ErlInt s, ErlInt 0]
>   where
>     (ms, s) = fromIntegral sec `divMod` 1000000
