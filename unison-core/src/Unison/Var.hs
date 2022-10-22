module Unison.Var
  ( Var (..),
    Type (..),
    InferenceType (..),
    blank,
    freshIn,
    inferAbility,
    inferInput,
    inferOther,
    inferOutput,
    inferPatternBindE,
    inferPatternBindV,
    inferPatternPureE,
    inferPatternPureV,
    inferTypeConstructor,
    inferTypeConstructorArg,
    joinDot,
    missingResult,
    name,
    nameStr,
    named,
    namespaced,
    rawName,
    reset,
    uncapitalize,
    universallyQuantifyIfFree,
    unnamedRef,
    unnamedTest,
  )
where

import Data.Char (isLower, toLower)
import qualified Data.List.NonEmpty as List.NonEmpty
import Data.Text (pack)
import qualified Data.Text as Text
import qualified Unison.ABT as ABT
import Unison.Name (Name)
import qualified Unison.Name as Name
import qualified Unison.NameSegment as NameSegment
import Unison.Prelude
import qualified Unison.Reference as Reference
import Unison.WatchKind (WatchKind, pattern TestWatch)

-- | A class for variables. Variables may have auxiliary information which
-- may not form part of their identity according to `Eq` / `Ord`. Laws:
--
--   * `typeOf (typed n) == n`
--   * `typeOf (ABT.freshIn vs v) == typeOf v`:
--     `ABT.freshIn` does not alter the name
class ABT.Var v => Var v where
  typed :: Type -> v
  typeOf :: v -> Type
  freshId :: v -> Word64
  freshenId :: Word64 -> v -> v

freshIn :: ABT.Var v => Set v -> v -> v
freshIn = ABT.freshIn

named :: Var v => Name -> v
named n = typed (User n)

rawName :: (Name -> Text) -> Type -> Text
rawName nameToText typ = case typ of
  User n -> nameToText n
  Inference Ability -> "𝕖"
  Inference Input -> "𝕒"
  Inference Output -> "𝕣"
  Inference Other -> "𝕩"
  Inference PatternPureE -> "𝕞"
  Inference PatternPureV -> "𝕧"
  Inference PatternBindE -> "𝕞"
  Inference PatternBindV -> "𝕧"
  Inference TypeConstructor -> "𝕗"
  Inference TypeConstructorArg -> "𝕦"
  MissingResult -> "_"
  Blank -> "_"
  Eta -> "_eta"
  ANFBlank -> "_anf"
  Float -> "_float"
  Pattern -> "_pattern"
  Irrelevant -> "_irrelevant"
  UnnamedReference ref -> Reference.idToText ref
  UnnamedWatch k guid -> fromString k <> "." <> guid

name :: Var v => (Name -> Text) -> v -> Text
name nameToText v = rawName nameToText (typeOf v) <> showid v
  where
    showid (freshId -> 0) = ""
    showid (freshId -> n) = pack (show n)

uncapitalize :: Var v => v -> v
uncapitalize v = undefined -- nameds $ go (nameStr v)
  where
    go (c : rest) = toLower c : rest
    go n = n

missingResult,
  blank,
  inferInput,
  inferOutput,
  inferAbility,
  inferPatternPureE,
  inferPatternPureV,
  inferPatternBindE,
  inferPatternBindV,
  inferTypeConstructor,
  inferTypeConstructorArg,
  inferOther ::
    Var v => v
missingResult = typed MissingResult
blank = typed Blank
inferInput = typed (Inference Input)
inferOutput = typed (Inference Output)
inferAbility = typed (Inference Ability)
inferPatternPureE = typed (Inference PatternPureE)
inferPatternPureV = typed (Inference PatternPureV)
inferPatternBindE = typed (Inference PatternBindE)
inferPatternBindV = typed (Inference PatternBindV)
inferTypeConstructor = typed (Inference TypeConstructor)
inferTypeConstructorArg = typed (Inference TypeConstructorArg)
inferOther = typed (Inference Other)

unnamedRef :: Var v => Reference.Id -> v
unnamedRef ref = typed (UnnamedReference ref)

unnamedTest :: Var v => Text -> v
unnamedTest guid = typed (UnnamedWatch TestWatch guid)

data Type
  = -- User provided variables, these should generally be left alone
    User Name
  | -- Variables created during type inference
    Inference InferenceType
  | -- Variables created to finish a block that doesn't end with an expression
    MissingResult
  | -- Variables invented for placeholder values inserted by user or by TDNR
    Blank
  | -- | An unnamed reference, created during unhashing a term/decl component.
    UnnamedReference Reference.Id
  | -- An unnamed watch expression of the given kind, for instance:
    --
    --  test> Ok "oog"
    --    has kind "test"
    --  > 1 + 1
    --    has kind ""
    UnnamedWatch WatchKind Text -- guid
    -- An unnamed variable for constructor eta expansion
  | Eta
  | -- An unnamed variable introduced by ANF transformation
    ANFBlank
  | -- An unnamed variable for a floated lambda
    Float
  | -- An unnamed variable introduced from pattern compilation
    Pattern
  | -- A variable for situations where we need to make up one that
    -- definitely won't be used.
    Irrelevant
  deriving (Eq, Ord)

data InferenceType
  = Ability
  | Input
  | Output
  | PatternPureE
  | PatternPureV
  | PatternBindE
  | PatternBindV
  | TypeConstructor
  | TypeConstructorArg
  | Other
  deriving (Eq, Ord, Show)

reset :: Var v => v -> v
reset v = typed (typeOf v)

namespaced :: Var v => [v] -> v
namespaced vs = undefined -- named $ intercalateMap "." name vs

nameStr :: Var v => (Name -> Text) -> v -> String
nameStr nameToText = Text.unpack . name nameToText

joinDot :: Var v => v -> v -> v
joinDot prefix v2 = undefined
  -- if name prefix == "."
  --   then named (name prefix `mappend` name v2)
  --   else named (name prefix `mappend` "." `mappend` name v2)

-- | Is this variable either:
--
--   * A user-written var (User) that's a relative, single-segment name (e.g. `foo`, but not `.bar`, nor `baz.qux`)
--   * An inference var
universallyQuantifyIfFree :: forall v. Var v => v -> Bool
universallyQuantifyIfFree v =
  case typeOf v of
    User n ->
      Name.isRelative n && case Name.reverseSegments n of
        seg List.NonEmpty.:| [] -> isLower (Text.head (NameSegment.toText seg))
        _ -> False
    Inference _ -> True
    _ -> False
