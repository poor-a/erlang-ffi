# erlang-ffi

Speaks the Erlang network protocol and impersonates an Erlang node on the network. Fully capable of bi-directional communication with Erlang. Erlang types are, as far as reasonable, mapped to Haskell types. Messages to Erlang are just function calls in Haskell, and messages from Erlang are delivered to MVars. 

## Short example

We introduce the `erlang` library through a simple working example. We are going to write a simple ''Hello, Erlang!'' program.

Lets start with the Haskell code. Here we initialize the ffi with `createSelf`, and create a mailbox with `createMBox`. Then we send our greeting with our pid to a registered process `p` running on Erlang node `erl@127.0.0.1`. We expect an answer, so we get it from the mailbox and print it out to the standard output.

~~~~{haskell}
import Foreign.Erlang

main :: IO ()
main = do
  self <- createSelf "haskell@localhost"
  mbox <- createMBox self
  mboxSend mbox (Long "erl" "127.0.0.1") (Right "p") (mboxSelf mbox, "Hello, Erlang!")
  answer <- mboxRecv mbox
  putStrLn ("Received: " ++ fromErlang answer)
~~~~

Alright, that wasn't so hard! Lets continue with the Erlang side. We are not going to do any fancy thing here neither. We register a process that receives a message and replies to it.

~~~~{erlang}
-module(hello).
-export([main/0]).

main() ->
    P = spawn(fun f/0),
    register(p, P).

f() ->
    receive
        {Hs, Msg} ->
            io:format("Received ~p~n", [Msg]),
            Hs ! "Hello, Haskell!"
    end.
~~~~

Lets start up the engines! We are ready to compile and run our programs.

~~~~
$ ghc Hello.erl
[1 of 1] Compiling Main             ( Hello.hs, Hello.o )
Linking Hello ...
$ erlc hello.erl
~~~~

Firstly, we start an Erlang node:

~~~~
$ erl -name "erl@127.0.0.1"
Eshell V6.2  (abort with ^G)
(erl@127.0.0.1)1> l(hello).
{module,hello}
(erl@127.0.0.1)2> hello:main().
true
~~~~

Erlang node is started, and the process is registered as well. Now start the Haskell program:

~~~~
./Hello 
Received: Hello, Haskell!
~~~~

Taking a look at the Erlang shell, we see that the message is successfully delivered:

~~~~
(erl@127.0.0.1)3> Received "Hello, Erlang!"
~~~~
