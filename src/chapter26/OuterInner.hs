module OuterInner where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

-- We only need to use return once,
-- because it's one big monad.
embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = return 1

maybeUnwrap :: ExceptT String (ReaderT () IO) (Maybe Int)
maybeUnwrap = runMaybeT embedded

-- Next
eitherUnwrap :: ReaderT () IO (Either String (Maybe Int))
eitherUnwrap = runExceptT maybeUnwrap

-- Lastly
readerUnwrap :: () -> IO (Either String (Maybe Int))
readerUnwrap = runReaderT eitherUnwrap

foo :: ReaderT () IO (Either String (Maybe Int))
foo = ReaderT $ \() -> return (Right (Just 1))

bar :: ExceptT String (ReaderT () IO) (Maybe Int)
bar = ExceptT $ return (Right (Just 1))

baz :: MaybeT (ExceptT String (ReaderT () IO)) Int
baz = undefined
