module LVar.Liveness where
import LVar.X86 (GenInstr (..), Arg (..), Reg (..), Program (Program), Block)
import Data.Set (Set)
import qualified Data.Set as Set
import UndirectedGraph (Graph)
import qualified UndirectedGraph
import qualified DirectedGraph
import Control.Monad (guard)
import Data.Map (Map)
import Data.Text (Text)
import Utils (mapFst, concatSetsMap, printBracketedSet)
import Data.Maybe (fromJust)
import qualified Data.Map as Map
import qualified LVar.X86 as X86
import qualified LVar.ASTMon as ASTMon
import qualified Data.Text as Text

argumentRegisters :: [Reg]
argumentRegisters = [Rdi, Rsi, Rdx, Rcx, R8, R9]

callerSaved :: [Reg]
callerSaved = [Rax, Rcx, Rdx, Rsi, Rdi, R8, R9, R10, R11]

calleeSaved :: Set Reg
calleeSaved = Set.fromList [Rsp, Rbp, Rbx, R12, R13, R14, R15]

instructionReads :: Ord n => GenInstr n -> Set (Arg n)
instructionReads = \case
  Addq l1 l2 -> Set.fromList [l1, l2]
  Subq l1 l2 -> Set.fromList [l1, l2]
  Xorq l1 l2 -> Set.fromList [l1, l2]
  Cmpq l1 l2 -> Set.fromList [l1, l2]
  Set _ _ -> Set.empty
  Movzbq l1 _ -> Set.singleton l1
  JumpIf _ _ -> Set.empty -- technically, EFLAGS, but we don't modify it directly anyway
  Movq src _ -> Set.singleton src
  Negq l1 -> Set.singleton l1
  Pushq l1 -> Set.singleton l1
  Popq _ -> Set.empty
  Callq _ arity -> Set.fromList (map Reg (take arity argumentRegisters))
  Retq -> Set.singleton (Reg Rax)
  Jump _ -> Set.empty

instructionWritesTo :: Ord n => GenInstr n -> Set (Arg n)
instructionWritesTo = \case
  Addq l1 l2 -> Set.singleton l2
  Subq l1 l2 -> Set.singleton l2
  Xorq l1 l2 -> Set.singleton l2
  Cmpq l1 l2 -> Set.empty
  Set _ l2 -> Set.singleton l2
  Movzbq _ l2 -> Set.singleton l2
  JumpIf _ _ -> Set.empty
  Movq src dest -> Set.singleton dest
  Negq l1 -> Set.singleton l1
  Pushq l1 -> Set.singleton (Reg Rsp)
  Popq l1 -> Set.fromList [l1, Reg Rsp]
  Callq _ arity -> Set.fromList (map Reg callerSaved)
  Retq -> Set.empty
  Jump _ -> Set.empty

beforeInstr :: Ord n => Map Text (LiveBlock n) -> GenInstr n -> Set (Arg n) -> Set (Arg n)
beforeInstr blockMap instr after =
  let w = instructionWritesTo instr
      r = instructionReads instr
      typical = Set.union (Set.difference after w) r
  in
    case instr of
      Jump label -> lbLiveBefore (fromJust (Map.lookup label blockMap))
      JumpIf _ label -> Set.union typical (lbLiveBefore (fromJust (Map.lookup label blockMap)))
      _ -> typical

data LiveBlock n = LiveBlock
  { lbStatements :: [(GenInstr n, Set (Arg n))]
  , lbLiveBefore :: Set (Arg n)
  }

printLiveBlock :: Ord n => (n -> Text) -> LiveBlock n -> Text
printLiveBlock n (LiveBlock ss before) =
  let printStmt (instr, liveAfter) = ["    " <> X86.rawPrintInstr n instr, "  " <> printBracketedSet (X86.printArg n) liveAfter] in
  Text.unlines ("  " <> printBracketedSet (X86.printArg n) before : concatMap printStmt ss)

printLiveBlockMap :: Ord n => (n -> Text) -> Map Text (LiveBlock n) -> Text
printLiveBlockMap n map =
  let printBlock (name, block) = [name <> ":", printLiveBlock n block] in
  Text.unlines (concatMap printBlock (Map.toList map))

computeLiveBlock :: Ord n => Map Text (LiveBlock n) -> Set (Arg n) -> [GenInstr n] -> LiveBlock n
computeLiveBlock blockMap liveStart instrs =
  let revInstrs = reverse instrs
      go curr = \case
        [] -> ([], curr)
        (instr : rest) -> mapFst ((instr, curr) :) (go (beforeInstr blockMap instr curr) rest)
      (revLiveAfters, liveBefore) = go liveStart revInstrs
  in LiveBlock (reverse revLiveAfters) liveBefore

computeLiveMap :: Ord n => [Text] -> DirectedGraph.Graph Text -> Map Text (Block n) -> Map Text (LiveBlock n)
computeLiveMap queue g blockMap =
  let go map q = case q of
        [] -> map
        hd : tl ->
          let followingBlocks = DirectedGraph.edgesList hd g
              block = fromJust (Map.lookup hd blockMap)
              -- fromJust should not fail, because following blocks should have been computed before
              lookupLive = lbLiveBefore . fromJust . flip Map.lookup map
              liveStart = concatSetsMap lookupLive followingBlocks
              liveBlock = computeLiveBlock map Set.empty block
          in go (Map.insert hd liveBlock map) tl
  in go (Map.singleton "conclusion" (LiveBlock [] Set.empty)) queue

interferenceGraph :: DirectedGraph.Graph Text -> [Text] -> X86.Program ASTMon.Name -> Graph (Arg ASTMon.Name)
interferenceGraph g queue program =
  let live = computeLiveMap queue g (X86.progBlocks program)
  in justInterferenceGraph (Map.elems live)

justInterferenceGraph :: Ord n => [LiveBlock n] -> Graph (Arg n)
justInterferenceGraph = foldr addToInterferenceGraph UndirectedGraph.empty

addToInterferenceGraph :: Ord n => LiveBlock n -> Graph (Arg n) -> Graph (Arg n)
addToInterferenceGraph instrs g =
  let fromLoc u = case u of
        Name _ -> [u]
        Reg _ -> [u]
        ByteReg _ -> []
        Deref _ _ -> error "should not be here during register allocation"
        Immediate _ -> []
      fromPair u v = do
        u1 <- fromLoc u
        v1 <- fromLoc v
        pure (u1, v1)
      addEdges instr liveAfter =
        case instr of
          Movq src dest -> do
            v <- Set.toList liveAfter
            guard (v /= src)
            guard (v /= dest)
            fromPair dest v
          Movzbq src dest -> do
            v <- Set.toList liveAfter
            guard (v /= src)
            guard (v /= dest)
            fromPair dest v
          _ -> do
            d <- Set.toList (instructionWritesTo instr)
            v <- Set.toList liveAfter
            guard (v /= d)
            fromPair d v
      addToGraph instr liveAfter g =
        foldr (uncurry UndirectedGraph.addEdge) g (addEdges instr liveAfter)
  in foldr (uncurry addToGraph) g (lbStatements instrs)
