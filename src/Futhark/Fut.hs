{-# LANGUAGE RankNTypes, ExistentialQuantification, FlexibleInstances, UndecidableInstances, TypeFamilies, MultiParamTypeClasses #-}
module Futhark.Fut (FutT, Fut, FutIO, runFutIn, runFutWith, runFut, runFutTIn, runFutTWith, runFutT, mapFutT, map2FutT, pureFut, unsafeFromFutIO, unsafeLiftFromIO) where
import Futhark.Context
import Futhark.Config
import System.IO.Unsafe
import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Identity
import Control.Monad.IO.Class


newtype FutT c m a = FutT (Context -> m a)

instance MonadTrans (FutT c) where
    lift a = FutT (\_ -> a)
    {-# INLINEABLE lift #-}

instance Functor m => Functor (FutT c m) where
    fmap f (FutT a) = FutT (fmap f.a)
    {-# INLINEABLE fmap #-}

instance Applicative m => Applicative (FutT c m) where
    pure a = FutT (\_ -> pure a)
    (<*>) (FutT a) (FutT b) = FutT (\c -> a c <*> b c)
    {-# INLINEABLE pure #-}
    {-# INLINEABLE (<*>) #-}

instance Monad m => Monad (FutT c m) where
    (>>=) (FutT a) f = FutT (\c -> a c >>= (\(FutT b) -> b c) . f)
    {-# INLINEABLE (>>=) #-}

instance MonadIO m => MonadIO (FutT c m) where
   liftIO = lift . liftIO
   {-# INLINEABLE liftIO #-}

instance (MonadBase b m) => MonadBase b (FutT c m) where
    liftBase = liftBaseDefault
    {-# INLINEABLE liftBase #-}

instance MonadTransControl (FutT c) where
    type StT (FutT c) a = a
    liftWith a = FutT (\c -> a (\(FutT a') -> a' c))
    restoreT = lift
    {-# INLINEABLE liftWith #-}
    {-# INLINEABLE restoreT #-}

instance MonadBaseControl b m => MonadBaseControl b (FutT c m) where
    type StM (FutT c m) a = ComposeSt (FutT c) m a
    liftBaseWith = defaultLiftBaseWith
    restoreM = defaultRestoreM
    {-# INLINEABLE liftBaseWith #-}
    {-# INLINEABLE restoreM #-}


type Fut c = FutT c Identity
type FutIO c = FutT c IO

mapFutT :: (m a -> n b) -> FutT c m a -> FutT c n b
mapFutT f (FutT a) = FutT (f.a)
map2FutT :: (m a -> n b -> k c) -> FutT c' m a -> FutT c' n b -> FutT c' k c
map2FutT f (FutT a) (FutT b) = FutT (\c -> f (a c) (b c))


runFutTIn :: Context -> (forall c. FutT c m a) -> m a
runFutTIn context (FutT a) = a context

runFutTWith :: [ContextOption] -> (forall c. FutT c m a) -> m a
runFutTWith options a
    = unsafePerformIO
    $ getContext options >>= \c -> return $ runFutTIn c a
runFutT = runFutTWith []

runFutIn :: Context -> (forall c. Fut c a) -> a
runFutIn context a = runIdentity $ runFutTIn context $ a

runFutWith :: [ContextOption] -> (forall c. Fut c a) -> a
runFutWith options a = runIdentity $ runFutTWith options a
runFut = runFutWith []

pureFut :: (Monad m) => Fut c a -> FutT c m a
pureFut (FutT a) = FutT (pure . runIdentity . a)

unsafeFromFutIO :: FutIO c a -> Fut c a
unsafeFromFutIO (FutT a) = FutT (Identity . unsafePerformIO . a)

unsafeLiftFromIO :: Monad m => (Context -> IO a) -> FutT c m a
unsafeLiftFromIO a = FutT (pure . unsafePerformIO . a)

