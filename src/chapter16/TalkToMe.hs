module TalkToMe where

data TalkToMe a = Halt
                | Print String a
                | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt        = Halt
  fmap f (Print s x) = Print s (f x)
  fmap f (Read g)    = Read (f . g)
