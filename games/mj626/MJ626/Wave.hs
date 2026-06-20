module MJ626.Wave where

data Wave = TRI | SIN | SQUARE | SAW | TAN
  deriving (Show, Eq, Ord, Enum, Bounded)
