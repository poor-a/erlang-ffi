# erlang-ffi

Speaks the Erlang network protocol and impersonates an Erlang node on the network. Fully capable of bi-directional communication with Erlang. Erlang types are, as far as reasonable, mapped to Haskell types. Messages to Erlang are just function calls in Haskell, and messages from Erlang are delivered to MVars. 
