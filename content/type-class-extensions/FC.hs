{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
-- {-# LANGUAGE UndecidableInstances #-}

module FC where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Data.Maybe (fromMaybe)

-- class Monad m => MonadReader r m | m -> r where
--   ask :: m r

-- instance MonadReader r ((->) r) where
--   ask = id

-- instance MonadReader r m => MonadReader r (MaybeT m) where
--   ask = lift ask

class Monad m => MonadReader r m | m -> r where
  ask :: m r

instance MonadReader r ((->) r) where
  ask = id

instance MonadReader r m => MonadReader r (MaybeT m)

incEnv ::
  (MonadReader r m, Num r)
  => m r
incEnv =
  (+ 1) <$> ask

test ::
  Integer
test =
  ($ 41) $ (+ 1) <$> ask



