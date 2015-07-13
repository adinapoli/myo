
module Myo.Types where

data Result =
  	Success
  | Error
  | InvalidArgument
	| RuntimeError
	deriving (Show, Eq, Ord)
