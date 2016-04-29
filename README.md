# Actor Model Example

Actors handle messages sent to them by sending messages to an address, spawning
other actors, or modifying their behavior with respect to future messages.
Actors are provided their own address when spawned to allow recursion.

## Language

```
data Actor a

unit :: Data
(:@) :: Data -> Data -> Data
instance Num Data
instance IsString Data

type Message = Data
type Address = Data
newtype Handler = Handler (Message -> Actor (Data, Handler))

instance Functor Actor
instance Monad Actor

spawn :: (Address -> Handler) -> Actor Address
send :: Address -> Message -> Actor ()
debug :: Show a => a -> Actor ()
runActor :: (Address -> Handler) -> IO a
```

## Example Actor

```
newAtom :: Address -> Handler
newAtom self = Handler (h unit) where
  h s ("swap" :@ x) = return (s, Handler (h x))
```

## Unbounded Nondeterminism

```
newUnbounded :: Address -> Handler
newUnbounded self = Handler (h 0) where
  h i "stop" = return (i, inactive self)
  h i "go" = do
    send self "go"
    return (unit, Handler (h (i+1)))

inactive :: Address -> Handler
inactive self = Handler (\_ -> return (unit, inactive self))

main :: Address -> Handler
main self = Handler $ \"start" -> do
  unbounded <- spawn newUnbounded
  send unbounded "go"
  i <- send unbounded "stop"
  debug "unbounded answer:"
  debug i
  return (unit, inactive self)
```
