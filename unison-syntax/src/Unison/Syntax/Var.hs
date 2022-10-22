{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Syntax-related combinators for Name (to/from string types).
module Unison.Syntax.Var
  ( joinDot,
    name,
    nameStr,
    named,
    nameds,
    namespaced,
    rawName,
    uncapitalize,
  )
where

import Data.Char (isLower, toLower)
import Data.Text (pack)
import qualified Data.Text as Text
import Unison.Name (Name)
import qualified Unison.Name as Name
import qualified Unison.NameSegment as Name
import Unison.Prelude
import qualified Unison.Reference as Reference
import Unison.Util.Monoid (intercalateMap)
import Unison.Var (Var)
import qualified Unison.Var as Var
import Unison.WatchKind (WatchKind, pattern TestWatch)

deriving instance Show Var.Type

named :: Var v => Name -> v
named n = Var.typed (User n)

name :: Var v => v -> Text
name v = rawName (typeOf v) <> showid v
  where
    showid (freshId -> 0) = ""
    showid (freshId -> n) = pack (show n)

uncapitalize :: Var v => v -> v
uncapitalize v = nameds $ go (nameStr v)
  where
    go (c : rest) = toLower c : rest
    go n = n

namespaced :: Var v => [v] -> v
namespaced vs = named $ intercalateMap "." name vs

nameStr :: Var v => v -> String
nameStr = Text.unpack . name

nameds :: Var v => String -> v
nameds s = named (Text.pack s)

joinDot :: Var v => v -> v -> v
joinDot prefix v2 =
  if name prefix == "."
    then named (name prefix `mappend` name v2)
    else named (name prefix `mappend` "." `mappend` name v2)
