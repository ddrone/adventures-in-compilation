module LVar.Compiler where

import Control.Monad.Reader
import Control.Monad.State
import Data.Int (Int64)
import Data.Map (Map)
import Data.Void (Void)
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.IntMap as IntMap

import DirectedGraph (topologicalSort)
import LVar.X86 (GenInstr(..), Reg(..))
import LVar.Liveness
import LVar.ExplicateControl (explicateControl)
import LVar.MoveBiasing (moveRelated)
import qualified UndirectedGraph
import qualified LVar.AST as AST
import qualified LVar.ASTC as ASTC
import qualified LVar.ASTMon as ASTMon
import qualified LVar.X86 as X86
import qualified DirectedGraph
import qualified Data.Text as Text

type RCO a = State Int a

shrinkExpr :: AST.Expr () -> AST.Expr ()
shrinkExpr = AST.exprTopdown $ \case
  AST.Bin AST.And e1 e2 -> AST.If e1 e2 ((), AST.Bool False)
  AST.Bin AST.Or e1 e2 -> AST.If e1 ((), AST.Bool False) e2
  e -> e

fresh :: RCO ASTMon.Name
fresh = do
  next <- get
  modify (+1)
  pure (ASTMon.Gen next)

rcoAtom :: AST.Expr a -> RCO (ASTMon.Atom, [ASTMon.Stmt])
rcoAtom e = case e of
  AST.Const n -> pure (ASTMon.Const n, [])
  AST.Bool b -> pure (ASTMon.Bool b, [])
  AST.Name v -> pure (ASTMon.Name (ASTMon.Source v), [])
  _ -> do
    (le, ls) <- rcoExpr e
    name <- fresh
    pure (ASTMon.Name name, ls ++ [ASTMon.Assign name le])

rcoCond :: AST.Expr a -> RCO (ASTMon.Cmp, [ASTMon.Stmt])
rcoCond e = case e of
  AST.Bin op (_, l) (_, r) | AST.isComparisonOp op -> do
    (la, ls) <- rcoAtom l
    (ra, rs) <- rcoAtom r
    pure (ASTMon.CmpOp op la ra, ls ++ rs)
  _ -> do
    (ea, es) <- rcoAtom e
    pure (ASTMon.CmpAtom ea, es)

rcoExpr :: AST.Expr a -> RCO (ASTMon.Expr, [ASTMon.Stmt])
rcoExpr e = case e of
  AST.Bin op (_, l) (_, r) -> do
    (la, ls) <- rcoAtom l
    (ra, rs) <- rcoAtom r
    pure (ASTMon.Bin op la ra, ls ++ rs)
  AST.Unary op (_, inner) -> do
    (ia, is) <- rcoAtom inner
    pure (ASTMon.Unary op ia, is)
  AST.InputInt ->
    pure (ASTMon.InputInt, [])
  AST.If (_, cond) (_, cons) (_, alt) -> do
    (cond1, condSs) <- rcoCond cond
    cons1 <- rcoBlock cons
    alt1 <- rcoBlock alt
    pure (ASTMon.If cond1 cons1 alt1, condSs)
  _ -> do
    (ea, es) <- rcoAtom e
    pure (ASTMon.Atom ea, es)
  where
    rcoBlock e = uncurry ASTMon.begin <$> rcoExpr e

rcoStmt :: AST.Stmt a -> RCO [ASTMon.Stmt]
rcoStmt = \case
  AST.Print (_, e) -> wrapAtom ASTMon.Print e
  AST.Calc (_, e) -> wrap ASTMon.Calc e
  AST.Assign (_, n) (_, e) -> wrap (ASTMon.Assign (ASTMon.Source n)) e
  AST.IfS (_, cond) (_, cons) mAlt -> do
    (ca, cs) <- rcoCond cond
    cons1 <- concat <$> mapM (rcoStmt . snd) cons
    alt1 <- case mAlt of
      Nothing -> pure []
      Just (_, alt) -> concat <$> mapM (rcoStmt . snd) alt
    pure (cs ++ [ASTMon.IfS ca cons1 alt1])
  AST.While (_, cond) (_, body) -> do
    (ca, cs) <- rcoCond cond
    body1 <- concat <$> mapM (rcoStmt . snd) body
    pure [ASTMon.While cs ca body1]
  where
    wrapAtom f e = do
      (ea, es) <- rcoAtom e
      pure (es ++ [f ea])
    wrap f e = do
      (ea, es) <- rcoExpr e
      pure (es ++ [f ea])

rcoModule :: AST.GenModule (AST.Stmt a) a -> (ASTMon.Module, Int)
rcoModule (AST.Module stmts) = flip runState 0 $ do
  newStmts <- concat <$> mapM (rcoStmt . snd) stmts
  pure (AST.Module $ map ((,) ()) newStmts)

peModule :: ASTMon.Module -> ASTMon.Module
peModule (AST.Module stmts) = AST.Module (map ((,) ()) $ ASTMon.partialEval (map snd stmts))

type Instr = X86.GenInstr ASTMon.Name

type Arg = X86.Arg ASTMon.Name

atom :: ASTMon.Atom -> Arg
atom = \case
  ASTMon.Const c -> X86.Immediate c
  ASTMon.Bool True -> X86.Immediate 1
  ASTMon.Bool False -> X86.Immediate 0
  ASTMon.Name n -> X86.Name n

comparisonBinops :: Map AST.Binop X86.Cmp
comparisonBinops = Map.fromList
  [ (AST.Le, X86.Le)
  , (AST.Lt, X86.L)
  , (AST.Ge, X86.Ge)
  , (AST.Gt, X86.G)
  , (AST.Eq, X86.E)
  , (AST.Ne, X86.Ne)
  ]

selectBinop :: AST.Binop -> Either (Arg -> Arg -> Instr) X86.Cmp
selectBinop op = case op of
  AST.Add -> Left Addq
  AST.Sub -> Left Subq
  _ -> case Map.lookup op comparisonBinops of
    Nothing -> error ("unexpected operator " ++ show op)
    Just cmp -> Right cmp

selectUnop :: AST.Unop -> Arg -> Instr
selectUnop = \case
  AST.Neg -> Negq
  AST.Not -> Xorq (X86.Immediate 1)

selectExpr :: Arg -> ASTC.Expr -> [Instr]
selectExpr dest = \case
  ASTC.Atom a ->
    let src = atom a in [Movq src dest | dest /= src]
  ASTC.Bin op a1 a2 ->
    let src1 = atom a1
        src2 = atom a2 in
    case selectBinop op of
      Left instr ->
        if
        | src1 == dest -> [ instr src2 dest ]
        | src2 == dest -> [ instr src1 dest ]
        | otherwise ->
          [ Movq src1 dest
          , instr src2 dest
          ]
      Right cmp ->
        [ Cmpq src1 src2
        , Set cmp (X86.ByteReg X86.Al)
        , Movzbq (X86.ByteReg X86.Al) dest
        ]
  ASTC.Unary op a ->
    let src = atom a in
    if dest == src
      then [ selectUnop op dest ]
      else
        [ Movq src dest
        , selectUnop op dest
        ]
  ASTC.InputInt ->
    case dest of
      X86.Reg Rax -> [ Callq "read_int" 0 ]
      _ ->
        [ Callq "read_int" 0
        , Movq (X86.Reg Rax) dest
        ]

cmpq :: X86.Arg n -> X86.Arg n -> X86.Cmp -> (X86.GenInstr n, X86.Cmp)
cmpq a1 a2 c = case a2 of
  X86.Immediate{} -> (Cmpq a2 a1, X86.oppositeCmp c)
  _ -> (Cmpq a1 a2, c)

selectCmp :: ASTC.Cond -> (Instr, X86.Cmp)
selectCmp = \case
  ASTC.AtomC a -> cmpq (X86.Immediate 1) (atom a) X86.E
  ASTC.CmpC op a1 a2 ->
    case Map.lookup op comparisonBinops of
      Nothing -> error $ "should not have used " ++ show op ++ " in comparison"
      Just c -> cmpq (atom a2) (atom a1) c

selectStmt :: ASTC.Stmt -> [Instr]
selectStmt = \case
  ASTC.Print n ->
    [ Movq (atom n) (X86.Reg Rdi)
    , Callq "print_int" 1
    ]
  ASTC.Calc e -> selectExpr (X86.Reg Rax) e
  ASTC.Assign n e -> selectExpr (X86.Name n) e

selectTail :: ASTC.Tail -> [Instr]
selectTail = \case
  ASTC.Return e -> selectExpr (X86.Reg Rax) e ++ [Jump "conclusion"]
  ASTC.Goto l -> [Jump (ASTC.printLabel l)]
  ASTC.CondJump cmp l1 l2 ->
    let (first, c) = selectCmp cmp in
    [ first
    , JumpIf c (ASTC.printLabel l1)
    , Jump (ASTC.printLabel l2)
    ]

selectBlock :: ASTC.Block -> [Instr]
selectBlock (ASTC.Block ss tail) = concatMap selectStmt ss ++ selectTail tail

selectInstructions :: ASTC.Module -> X86.Program ASTMon.Name
selectInstructions (ASTC.Module start blocks) =
  let select (blockId, block) = (ASTC.printLabel blockId, selectBlock block)
      selected = Map.fromList (map select ((0, start) : IntMap.toList blocks)) in
  X86.Program (ASTC.printLabel 0) selected

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
  X86.ByteReg r -> pure (X86.ByteReg r)
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
  , ahInstructions :: X86.Program Void
  }

printInterferenceGraph = UndirectedGraph.printGraphSimple (Text.pack . show . X86.printArg ASTMon.printName)

collectArgument :: Ord n => X86.Arg n -> Set (X86.Arg n) -> Set (X86.Arg n)
collectArgument = \case
  a@(X86.Name n) -> Set.insert a
  _ -> id

collectLocations :: Ord n => X86.GenInstr n -> Set (X86.Arg n) -> Set (X86.Arg n)
collectLocations instr start = foldr collectArgument start (X86.getArgs instr)

assignHomesAndCountVars :: DirectedGraph.Graph Text -> [Text] -> X86.Program ASTMon.Name -> AssignHomesResult
assignHomesAndCountVars graph blockOrder program = do
  let ig = interferenceGraph graph (reverse blockOrder) program
  let mr = moveRelated (Map.elems (X86.progBlocks program))
  let namesToColor = X86.foldProgramInstrs collectLocations Set.empty program
  let colors = UndirectedGraph.moveBiasedSaturationColoring namesToColor initialColors mr ig
  let locations = colorsToLocMapping colors
  let maxColor = maximum (Map.elems colors)
  let stackLocs =
        if maxColor < length raRegisters
          then 0
          else (maxColor - length raRegisters) + 1
  let result = runReader (X86.mapProgramM ahInstr program) locations
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
  Movq src@X86.Deref{} dest@X86.Deref{} -> useIntermediate src dest Movq
  Addq src@X86.Deref{} dest@X86.Deref{} -> useIntermediate src dest Addq
  Subq src@X86.Deref{} dest@X86.Deref{} -> useIntermediate src dest Subq
  Xorq src@X86.Deref{} dest@X86.Deref{} -> useIntermediate src dest Xorq
  Movq src@(X86.Immediate n) dest@X86.Deref{} | n > immediateLimit ->
    useIntermediate src dest Movq
  Addq src@(X86.Immediate n) dest@X86.Deref{} | n > immediateLimit ->
    useIntermediate src dest Addq
  Subq src@(X86.Immediate n) dest@X86.Deref{} | n > immediateLimit ->
    useIntermediate src dest Subq
  -- Comparisons that are used in conditionals are inverted in the case if
  -- second parameter is an immediate, but second parameter being immediate
  -- can still appear when comparison is saved to a variable.
  Cmpq src dest@X86.Immediate{} ->
    -- Not using `useIntermediate` here because the restriction is on the second argument
    [ Movq dest rax
    , Cmpq src rax
    ]
  other -> [other]
  where
    useIntermediate src dest instr = [ Movq src rax, instr rax dest ]

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
