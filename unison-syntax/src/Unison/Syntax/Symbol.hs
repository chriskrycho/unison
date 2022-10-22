{-# OPTIONS_GHC -fno-warn-orphans #-}

module Unison.Syntax.Symbol () where

import Unison.Symbol
import Unison.Syntax.Name () -- instance Show Name

instance Show Symbol where
  show (Symbol 0 n) = show n
  show (Symbol id n) = show n ++ "-" ++ show id
