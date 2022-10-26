{-# LANGUAGE OverloadedStrings #-}

module Scotty where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
  hiding (get)
import Control.Monad.Trans.Reader
import Data.Monoid (mconcat)
import Web.Scotty
import Web.Scotty.Internal.Types
  (ActionT(..))

main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"

    (ActionT
     . (ExceptT . liftM Right)
     . ReaderT . const
     . \m -> StateT (\s -> do
                        a <- m
                        return (a, s))
     ) (putStrLn "hello")

    html $
      mconcat [ "<h1>Scotty, "
              , beam
              , " me up</h1>" ]
