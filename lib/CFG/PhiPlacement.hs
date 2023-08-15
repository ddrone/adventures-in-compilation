module CFG.PhiPlacement where

import Control.Monad.ST (runST)
import Data.Foldable (toList)
import Data.Text (Text)
import Data.Set (Set)
import Data.Map (Map)
import Data.STRef
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

type VariableMap = Map BlockName [Ident]

addBlock :: Block -> VariableMap -> VariableMap
addBlock block map =
  let toAdd = foldr addInstr Set.empty (blockAssigns block) in
  Map.insert (blockName block) (toList toAdd) map

addFunction :: Function -> VariableMap -> VariableMap
addFunction fn used = foldr addBlock used (fnBody fn)

usedVariables :: [Function] -> VariableMap
usedVariables fns = foldr addFunction Map.empty fns

-- For a block with a particular name, which identifiers should have
-- phi-nodes inserted when generating the SSA form.
type PhiMap = Map BlockName [Ident]

phiPlacement :: Function -> PhiMap
phiPlacement = runST $ do
  undefined
