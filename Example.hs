{-# LANGUAGE OverloadedStrings #-}
module Example where

import Actor
import Control.Monad.Trans (liftIO)

inactive :: Address -> Handler
inactive self = Handler (\_ -> return (unit, inactive self))

newAtom :: Address -> Handler
newAtom self = Handler (h unit) where
  h s ("swap" :@ x) = return (s, Handler (h x))

newRng :: Address -> Handler
newRng self = Handler (h 0) where
  h s ("seed" :@ i) = return (unit, Handler (h i))
  h s "rand" = do
    let u = (1664525*s + 1013904223) `mod` 2^32
    return (s, Handler (h u))

test :: Address -> Handler
test self = Handler $ \"start" -> do
  debug self
  rng <- spawn newRng
  atom <- spawn newAtom
  send rng ("seed" :@ 1000)
  i <- send rng "rand"
  debug i
  i <- send rng "rand"
  debug i
  x <- send atom ("swap" :@ 9)
  debug x
  x <- send atom ("swap" :@ 13)
  debug x
  return (unit, inactive self)
