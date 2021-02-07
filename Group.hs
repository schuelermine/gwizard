{-# LANGUAGE StandaloneDeriving, DerivingStrategies, GeneralizedNewtypeDeriving #-}

module Group (Group(), mkGroup) where

newtype Group = Group Integer

deriving newtype instance Show Group
deriving newtype instance Read Group
deriving instance Eq Group
deriving instance Ord Group

instance Num Group where
    Group a + Group b = mkGroup (a + b)
    Group a * Group b = mkGroup (a * b)
    abs = id
    signum (Group _) = Group 1
    negate (Group a) = mkGroup $ negate a
    fromInteger = mkGroup

mkGroup :: Integer -> Group
mkGroup a = Group $ mod a 4