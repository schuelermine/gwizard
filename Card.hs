{-# LANGUAGE StandaloneDeriving #-}

module Card (Card(..), mkCard) where

import Value
import Group

data Card
    = Wizard
    | Jester
    | Dragon
    | Fairy
    | Bomb
    | Werewolf
    | Cloud
    | Juggler
    | Card Value Group

deriving instance Show Card
deriving instance Read Card
deriving instance Eq Card

mkCard :: Integer -> Integer -> Card
mkCard x y = Card (mkValue x) (mkGroup y)