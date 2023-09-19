module LVar.Compiler where

import Control.Monad.Reader
import Control.Monad.State
import Data.Int (Int64)
import Data.Map (Map)
import Data.Void (Void)
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

import LVar.X86 (GenInstr(..), Reg(..))
import LVar.Liveness
import LVar.MoveBiasing (moveRelated)
import qualified UndirectedGraph
import qualified LVar.AST as AST
import qualified LVar.ASTMon as ASTMon
import qualified LVar.X86 as X86

type RCO a = State Int a

shrinkExpr :: AST.Expr -> AST.Expr
shrinkExpr = AST.exprTopdown $ \case
  AST.Bin AST.And e1 e2 -> AST.If e1 e2 (AST.Bool False)
  AST.Bin AST.Or e1 e2 -> AST.If e1 (AST.Bool False) e2
  e -> e

fresh :: RCO ASTMon.Name
fresh = do
  next <- get
  modify (+1)
  pure (ASTMon.Gen next)

rcoAtom :: AST.Expr -> RCO (ASTMon.Atom, [ASTMon.Stmt])
rcoAtom e = case e of
  AST.Const n -> pure (ASTMon.Const n, [])
  AST.Bool b -> pure (ASTMon.Bool b, [])
  AST.Name v -> pure (ASTMon.Name (ASTMon.Source v), [])
  _ -> do
    (le, ls) <- rcoExpr e
    name <- fresh
    pure (ASTMon.Name name, ls ++ [ASTMon.Assign name le])

rcoCond :: AST.Expr -> RCO (ASTMon.Cmp, [ASTMon.Stmt])
rcoCond e = case e of
  AST.Bin op l r | AST.isComparisonOp op -> do
    (la, ls) <- rcoAtom l
    (ra, rs) <- rcoAtom r
    pure (ASTMon.CmpOp op la ra, ls ++ rs)
  AST.Bool b -> pure (ASTMon.CmpLit b, [])
  _ -> do
    (ea, es) <- rcoAtom e
    pure (ASTMon.CmpAtom ea, es)

rcoExpr :: AST.Expr -> RCO (ASTMon.Expr, [ASTMon.Stmt])
rcoExpr e = case e of
  AST.Bin op l r -> do
    (la, ls) <- rcoAtom l
    (ra, rs) <- rcoAtom r
    pure (ASTMon.Bin op la ra, ls ++ rs)
  AST.Unary op inner -> do
    (ia, is) <- rcoAtom inner
    pure (ASTMon.Unary op ia, is)
  AST.InputInt ->
    pure (ASTMon.InputInt, [])
  AST.If cond cons alt -> do
    (cond1, condSs) <- rcoCond cond
    cons1 <- rcoBlock cons
    alt1 <- rcoBlock alt
    pure (ASTMon.If cond1 cons1 alt1, condSs)
  _ -> do
    (ea, es) <- rcoAtom e
    pure (ASTMon.Atom ea, es)
  where
    rcoBlock e = uncurry ASTMon.begin <$> rcoExpr e

rcoStmt :: AST.Stmt -> RCO [ASTMon.Stmt]
rcoStmt = \case
  AST.Print e -> wrapAtom ASTMon.Print e
  AST.Calc e -> wrap ASTMon.Calc e
  AST.Assign n e -> wrap (ASTMon.Assign (ASTMon.Source n)) e
  AST.IfS cond cons alt -> do
    (ca, cs) <- rcoCond cond
    cons1 <- concat <$> mapM rcoStmt cons
    alt1 <- concat <$> mapM rcoStmt alt
    pure (cs ++ [ASTMon.IfS ca cons1 alt1])
  where
    wrapAtom f e = do
      (ea, es) <- rcoAtom e
      pure (es ++ [f ea])
    wrap f e = do
      (ea, es) <- rcoExpr e
      pure (es ++ [f ea])

rcoModule :: AST.Module -> (ASTMon.Module, Int)
rcoModule (AST.Module stmts) = flip runState 0 $ do
  newStmts <- concat <$> mapM rcoStmt stmts
  pure (AST.Module newStmts)

peModule :: ASTMon.Module -> ASTMon.Module
peModule (AST.Module stmts) = AST.Module (ASTMon.partialEval stmts)

type Instr = X86.GenInstr ASTMon.Name

type Arg = X86.Arg ASTMon.Name

atom :: ASTMon.Atom -> Arg
atom = \case
  ASTMon.Const c -> X86.Immediate c
  ASTMon.Name n -> X86.Name n

selectBinop :: AST.Binop -> Arg -> Arg -> Instr
selectBinop = \case
  AST.Add -> Addq
  AST.Sub -> Subq

selectUnop :: AST.Unop -> Arg -> Instr
selectUnop = \case
  AST.Neg -> Negq

selectExpr :: Arg -> ASTMon.Expr -> [Instr]
selectExpr dest = \case
  ASTMon.Atom a ->
    let src = atom a in
    if dest == src
      then []
      else [Movq src dest]
  ASTMon.Bin op a1 a2 ->
    let src1 = atom a1
        src2 = atom a2 in
    if
      | src1 == dest -> [ selectBinop op src2 dest ]
      | src2 == dest -> [ selectBinop op src1 dest ]
      | otherwise ->
        [ Movq src1 dest
        , selectBinop op src2 dest
        ]
  ASTMon.Unary op a ->
    let src = atom a in
    if dest == src
      then [ selectUnop op dest ]
      else
        [ Movq src dest
        , selectUnop op dest
        ]
  ASTMon.InputInt ->
    case dest of
      X86.Reg Rax -> [ Callq "read_int" 0 ]
      _ ->
        [ Callq "read_int" 0
        , Movq (X86.Reg Rax) dest
        ]

selectStmt :: ASTMon.Stmt -> [Instr]
selectStmt = \case
  ASTMon.Print n ->
    [ Movq (atom n) (X86.Reg Rdi)
    , Callq "print_int" 1
    ]
  ASTMon.Calc e -> selectExpr (X86.Reg Rax) e
  ASTMon.Assign n e -> selectExpr (X86.Name n) e

selectInstructions :: ASTMon.Module -> [Instr]
selectInstructions (AST.Module stmts) = concatMap selectStmt stmts

raRegisters :: [X86.Reg]
raRegisters =
  [ X86.Rcx
  , X86.Rdx
  , X86.Rsi
  , X86.Rdi
  , X86.R8
  , X86.R9
  , X86.R10
  , X86.R11
  , X86.R12
  , X86.R13
  , X86.R14
  ]

initialColors = Map.fromList (zip (map X86.Reg raRegisters) [0..])

data Location
  = LocReg Reg
  | LocStack Int
  deriving (Show)

-- Really slow, but writing faster function would be either tedious or require
-- Template Haskell, I'll probably go with the second option if I'd want to
-- speed this one up.
colorToLocation :: Int -> Location
colorToLocation = go raRegisters
  where
    go regs n = case regs of
      [] -> LocStack n
      hd : tl ->
        if n == 0
          then LocReg hd
          else go tl (n - 1)

locationToX86 :: Location -> X86.Arg Void
locationToX86 = \case
  LocReg r -> X86.Reg r
  LocStack i -> X86.Deref Rbp (-8 * (fromIntegral i + 1))

-- AH stands for "assign homes" monad
type AH a = Reader (Map ASTMon.Name Location) a

getLocation :: ASTMon.Name -> AH Location
getLocation name = do
  result <- asks (Map.lookup name)
  pure (fromJust result)

ahArg :: Arg -> AH (X86.Arg Void)
ahArg = \case
  X86.Name n -> do
    locationToX86 <$> getLocation n
  X86.Immediate i -> pure (X86.Immediate i)
  X86.Reg r -> pure (X86.Reg r)
  X86.Deref r o -> pure (X86.Deref r o)

ahInstr :: Instr -> AH (X86.GenInstr Void)
ahInstr = X86.traverseInstr ahArg

colorsToLocMapping colors =
  let addLoc arg color map =
        case arg of
          X86.Name n -> Map.insert n (colorToLocation color) map
          _ -> map
  in foldr (uncurry addLoc) Map.empty (Map.toList colors)

data AssignHomesResult = AssignHomesResult
  { ahStackLocations :: Int
  , ahUsedCalleeSavedRegisters :: Set X86.Reg
  , ahInstructions :: [X86.GenInstr Void]
  }

assignHomesAndCountVars :: [Instr] -> AssignHomesResult
assignHomesAndCountVars instrs = do
  let ig = interferenceGraph instrs
  let mr = moveRelated instrs
  let colors = UndirectedGraph.moveBiasedSaturationColoring (UndirectedGraph.allNodes ig) initialColors mr ig
  let locations = colorsToLocMapping colors
  let maxColor = maximum (Map.elems colors)
  let stackLocs =
        if maxColor < length raRegisters
          then 0
          else (maxColor - length raRegisters) + 1
  let result = runReader (mapM ahInstr instrs) locations
  let addReg loc set =
        case loc of
          LocReg r | Set.member r calleeSaved -> Set.insert r set
          _ -> set
  -- Callee-saved registers
  let csr = foldr addReg Set.empty (Map.elems locations)
  AssignHomesResult stackLocs csr result

immediateLimit :: Int64
immediateLimit = 2 ^ 16

rax = X86.Reg Rax

patchInstruction :: X86.GenInstr Void -> [X86.GenInstr Void]
patchInstruction = \case
  Movq src dest | src == dest -> []
  Movq src@X86.Deref{} dest@X86.Deref{} ->
    [ Movq src rax
    , Movq rax dest
    ]
  Addq src@X86.Deref{} dest@X86.Deref{} ->
    [ Movq src rax
    , Addq rax dest
    ]
  Subq src@X86.Deref{} dest@X86.Deref{} ->
    [ Movq src rax
    , Subq rax dest
    ]
  Movq src@(X86.Immediate n) dest@X86.Deref{} | n > immediateLimit ->
    [ Movq src rax
    , Movq rax dest
    ]
  Addq src@(X86.Immediate n) dest@X86.Deref{} | n > immediateLimit ->
    [ Movq src rax
    , Addq rax dest
    ]
  Subq src@(X86.Immediate n) dest@X86.Deref{} | n > immediateLimit ->
    [ Movq src rax
    , Subq rax dest
    ]
  other -> [other]

patchInstructions :: [X86.GenInstr Void] -> [X86.GenInstr Void]
patchInstructions = concatMap patchInstruction

roundUpEven x = x + x `mod` 2

generateWrapper :: Set Reg -> Int -> ([X86.GenInstr Void], [X86.GenInstr Void])
generateWrapper savedRegisters localsCount =
  let wordsSaved = roundUpEven (localsCount + Set.size savedRegisters)
      stackIncrease = fromIntegral (8 * (wordsSaved - Set.size savedRegisters))
      registerList = Set.toList savedRegisters
      prefix =
        [ Pushq (X86.Reg Rbp) ] ++
        map (Pushq . X86.Reg) registerList ++
        [ Movq (X86.Reg Rsp) (X86.Reg Rbp)
        , Subq (X86.Immediate stackIncrease) (X86.Reg Rsp)
        ]
      suffix =
        [ Addq (X86.Immediate stackIncrease) (X86.Reg Rsp) ] ++
        map (Popq . X86.Reg) (reverse registerList) ++
        [ Popq (X86.Reg Rbp)
        , Retq
        ]
  in (prefix, suffix)

compileAll :: AST.Module -> [X86.GenInstr Void]
compileAll mod =
  let (rco, _) = rcoModule (AST.mapModule shrinkExpr mod)
      pevaled = peModule rco
      selected = selectInstructions pevaled
      AssignHomesResult count csr assigned = assignHomesAndCountVars selected
      patched = patchInstructions assigned
      (prefix, suffix) = generateWrapper csr count
  in prefix ++ patched ++ suffix
