module Asset.Internal where

class HasInfo a where
  info :: a -> String

class HasStringID a where
  toAsset :: String -> Maybe a
  fromAsset :: a -> String

class IsPickable a where
  isPickable :: a -> Bool

class ResponseMessage a where
  success :: a -> String
  failuer :: a -> String


