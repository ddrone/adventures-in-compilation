module Free where

data Free f a
  = Pure a
  | Nest (f (Free f a))

instance Functor f => Functor (Free f) where
  fmap f = \case
    Pure x -> Pure (f x)
    Nest fx -> Nest (fmap (fmap f) fx)

instance Functor f => Applicative (Free f) where
  pure = Pure

  af <*> ax = case af of
    Pure f -> fmap f ax
    Nest ff -> Nest (fmap (<*> ax) ff)

instance Functor f => Monad (Free f) where
  mx >>= mf = case mx of
    Pure x -> mf x
    Nest fx -> Nest (fmap (>>= mf) fx)
