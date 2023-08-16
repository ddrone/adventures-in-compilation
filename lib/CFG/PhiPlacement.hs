module CFG.PhiPlacement where

import Control.Monad (filterM, forM_, when)
import Control.Monad.ST (runST)
import Data.Foldable (toList)
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text)
import Data.Set (Set)
import Data.Map (Map)
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.STRef
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet

import AST (Ident)
import CFG.Instr
import CFG.Printer (printBlockName)
import Graph.DominationFrontier (dominationFrontier)
import Graph.Parser (NodeDef (NodeDef), ParsedGraph (pgIds), buildGraph)
import Utils (regularMultimapInsert)
import Control.Arrow ((&&&))

addGenName :: Set Ident -> GenName -> Set Ident
addGenName used gn = case gn of
  Src v -> Set.insert v used
  _ -> used

addInstr :: Assign -> Set Ident -> Set Ident
addInstr assn used = addGenName used (asgnTarget assn)

type VariableMap = Map Ident [BlockName]

addBlock :: Block -> VariableMap -> VariableMap
addBlock block map =
  let toAdd = foldr addInstr Set.empty (blockAssigns block) in
  foldr (flip regularMultimapInsert (blockName block)) map toAdd

usedVariables :: Function -> VariableMap
usedVariables fn = foldr addBlock Map.empty (fnBody fn)

-- For a block with a particular name, which identifiers should have
-- phi-nodes inserted when generating the SSA form.
type PhiMap = Map BlockName [Ident]

exitNodes :: BlockEnd -> [BlockName]
exitNodes be = case be of
  Ret _ -> []
  Jump out -> [out]
  CondJump _ cons alt -> [cons, alt]

blockToNodeDef :: Block -> NodeDef
blockToNodeDef block = do
  let nodeName = printBlockName (blockName block)
  let nodeOuts = map printBlockName (exitNodes (blockEnd block))
  NodeDef nodeName nodeOuts

functionToParsedGraph :: Function -> ParsedGraph
functionToParsedGraph fn = do
  let root = printBlockName (fnStartLabel fn)
  let nodes = map blockToNodeDef (fnBody fn)
  buildGraph root nodes

phiPlacement :: Function -> PhiMap
phiPlacement fn = runST $ do
  let pg = functionToParsedGraph fn
  let df = dominationFrontier pg
  let vars = Map.assocs (usedVariables fn)
  phiMap <- newSTRef Map.empty
  let blockId bname = fromJust (Map.lookup (printBlockName bname) (pgIds pg))
  let allBlocks = IntMap.fromList $ map (blockId &&& id) $ map blockName (fnBody fn)
  let blockName id = fromJust (IntMap.lookup id allBlocks)
  let
    handleVar (v, blocks) = do
      domFrontPlus <- newSTRef IntSet.empty
      visited <- newSTRef IntSet.empty
      let
        dfs queue = case queue of
          first : rest -> do
            let nexts = IntSet.toList (fromMaybe IntSet.empty $ IntMap.lookup first df)
            let shouldBeAddedToQueue n = do
                  inDFP <- IntSet.member n <$> readSTRef domFrontPlus
                  inVisited <- IntSet.member n <$> readSTRef visited
                  pure (not inDFP && not inVisited)
            queueAdd <- filterM shouldBeAddedToQueue nexts
            forM_ nexts $ \next -> do
              inDFP <- IntSet.member next <$> readSTRef domFrontPlus
              when (not inDFP) $ do
                modifySTRef domFrontPlus $ IntSet.insert next
                modifySTRef phiMap $ regularMultimapInsert (blockName next) v
            modifySTRef visited $ \v -> foldr IntSet.insert v nexts
            dfs (queueAdd ++ rest)
          [] -> pure ()
      dfs (map blockId blocks)
  mapM_ handleVar vars
  readSTRef phiMap
