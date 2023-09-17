module Main where

import LVar.Parser (program)
import qualified Data.Text.IO as TextIO
import System.Environment (getArgs)
import Data.Foldable (forM_)
import Text.Megaparsec (runParser)
import LVar.Compiler (compileAll)
import LVar.X86 (printProgram, GenInstr (..), Arg (..))
import System.FilePath
import System.Process
import Data.Text (Text)
import LVar.X86 (Reg(..), printArg)
import LVar.Liveness (interferenceGraph)
import UndirectedGraph
import qualified Data.Map as Map

type Instr = GenInstr Text

v = Name "v"
w = Name "w"
x = Name "x"
y = Name "y"
z = Name "z"
tmp_0 = Name "tmp_0"
tmp_1 = Name "tmp_1"

testProgram :: [Instr]
testProgram =
  [ Movq (Immediate 1) v
  , Movq (Immediate 42) w
  , Movq v x
  , Addq (Immediate 7) x
  , Movq x y
  , Movq x z
  , Addq w z
  , Movq y tmp_0
  , Negq tmp_0
  , Movq z tmp_1
  , Addq tmp_0 tmp_1
  , Movq tmp_1 (Reg Rdi)
  , Callq "print_int" 1
  ]

nodeName :: Arg Text -> Text
nodeName arg = "\"" <> printArg id arg <> "\""

main = do
  let ig = interferenceGraph testProgram
  let coloring = saturationColoring (UndirectedGraph.allNodes ig) Map.empty ig
  mapM_ print (Map.toList coloring)
  print (isColoringValid (printArg id) coloring ig)

  files <- getArgs
  forM_ files $ \file -> do
    let assemblyName = replaceExtensions file "s"
    let binaryName = replaceExtensions file "exe"
    contents <- TextIO.readFile file
    case runParser program file contents of
      Left err -> print err
      Right p -> do
        let instrs = compileAll p
        let code = printProgram instrs
        TextIO.writeFile assemblyName code
        callProcess "cc"
          [ "-m64"
          , "-o"
          , binaryName
          , assemblyName
          , "tests/runtime.o"
          ]
