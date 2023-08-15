module CFG.PhiPlacement where

import Data.Foldable (toList)
import Data.Text (Text)
import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

import AST (Ident)
import CFG.Instr

addGenName :: Set Ident -> GenName -> Set Ident
addGenName used gn = case gn of
  Src v -> Set.insert v used
  _ -> used

addInstr :: Assign -> Set Ident -> Set Ident
addInstr assn used = addGenName used (asgnTarget assn)

addBlock :: Block -> Set Ident -> Set Ident
addBlock block used = foldr addInstr used (blockAssigns block)

addFunction :: Function -> Set Ident -> Set Ident
addFunction fn used = foldr addBlock used (fnBody fn)

usedVariables :: [Function] -> [Ident]
usedVariables fns = toList (foldr addFunction Set.empty fns)
