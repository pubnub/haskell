## PubNub Haskell Client

### Building

```
$ cabal configure
$ cabal install --only-dependencies
$ cabal build
```

### Run Examples

After building you can run the executable of the examples.

```
$ dist/build/hello_world/hello_world
Just (SubscribeResponse (Array (fromList []),Timestamp 13878501585466665))
Just (PublishResponse 1 "Sent" (Timestamp 13878501588199833))
```

Run the chat example from two different terminals:

```
$ dist/build/chat/chat
Enter Username:
tristan
hello
<tristan> : hello
<ellen> : oh hey
```

```
$ dist/build/chat/chat
Enter Username:
ellen
oh hey
<ellen> : oh hey
```
