{-# LANGUAGE StandaloneDeriving, DerivingStrategies, GeneralizedNewtypeDeriving #-}

module Value (Value(), mkValue) where

newtype Value = Value Integer

deriving newtype instance Show Value
deriving newtype instance Read Value
deriving instance Eq Value
deriving instance Ord Value

instance Num Value where
    Value a + Value b = mkValue (a + b)
    Value a * Value b = mkValue (a * b)
    abs = id
    signum (Value _) = Value 1
    negate (Value a) = mkValue $ negate a
    fromInteger = mkValue

mkValue :: Integer -> Value
mkValue a = Value $ mod a 13