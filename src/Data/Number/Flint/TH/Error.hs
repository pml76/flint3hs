module Data.Number.Flint.TH.Error (
    ErrorString(..)
) where 

data ErrorString = ErrorString String
  deriving (Eq, Show)