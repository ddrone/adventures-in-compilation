module LVar.Liveness where
import LVar.X86 (GenInstr (..), Arg (Reg), Reg (..))
import Data.Set (Set)
import qualified Data.Set as Set
import UndirectedGraph (Graph)
import qualified UndirectedGraph
import Control.Monad (guard)

argumentRegisters :: [Reg]
argumentRegisters = [Rdi, Rsi, Rdx, Rcx, R8, R9]

callerSaved :: [Reg]
callerSaved = [Rax, Rcx, Rdx, Rsi, Rdi, R8, R9, R10, R11]

instructionReads :: Ord n => GenInstr n -> Set (Arg n)
instructionReads = \case
  Addq l1 l2 -> Set.fromList [l1, l2]
  Subq l1 l2 -> Set.fromList [l1, l2]
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
  Movq src dest -> Set.singleton dest
  Negq l1 -> Set.singleton l1
  Pushq l1 -> Set.singleton (Reg Rsp)
  Popq l1 -> Set.fromList [l1, Reg Rsp]
  Callq _ arity -> Set.fromList (map Reg callerSaved)
  Retq -> Set.empty
  Jump _ -> Set.empty

beforeInstr :: Ord n => GenInstr n -> Set (Arg n) -> Set (Arg n)
beforeInstr instr after =
  let w = instructionWritesTo instr
      r = instructionReads instr in
  Set.union (Set.difference after w) r

computeLiveSets :: Ord n => [GenInstr n] -> [Set (Arg n)]
computeLiveSets instrs =
  let revInstrs = reverse instrs
      go curr = \case
        [] -> []
        (instr : rest) -> curr : go (beforeInstr instr curr) rest
  in reverse (go Set.empty revInstrs)

interferenceGraph :: Ord n => [GenInstr n] -> Graph (Arg n)
interferenceGraph instrs =
  let addEdges instr liveAfter =
        case instr of
          Movq src dest -> do
            v <- Set.toList liveAfter
            guard (v /= src)
            guard (v /= dest)
            pure (dest, v)
          _ -> do
            d <- Set.toList (instructionWritesTo instr)
            v <- Set.toList liveAfter
            guard (v /= d)
            pure (d, v)
      addToGraph instr liveAfter g =
        foldr (uncurry UndirectedGraph.addEdge) g (addEdges instr liveAfter)
  in foldr (uncurry addToGraph) UndirectedGraph.empty (zip instrs (computeLiveSets instrs))
