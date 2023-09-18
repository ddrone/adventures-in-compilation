module LVar.MoveBiasing where
import UndirectedGraph (Graph)
import LVar.X86 (GenInstr (Movq), Arg (Name))
import qualified UndirectedGraph as Graph

moveRelated :: Ord n => [GenInstr n] -> Graph n
moveRelated instrs =
  let addIntsr instr g =
        case instr of
          Movq (Name x) (Name y) -> Graph.addEdge x y g
          _ -> g
  in foldr addIntsr Graph.empty instrs
