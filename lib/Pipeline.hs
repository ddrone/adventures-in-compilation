module Pipeline where

import Data.Text (Text)
import Free (Free (..))
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import System.Exit (exitFailure)
import System.FilePath (replaceExtensions)

data PipelineF a
  = Abort Text
  | Emit String Text a
  deriving (Functor)

type Pipeline a = Free PipelineF a

abort :: Text -> Pipeline a
abort = Nest . Abort

emit :: String -> Text -> Pipeline ()
emit ext output = Nest (Emit ext output (Pure ()))

runPure :: Pipeline a -> Either Text a
runPure = \case
  Pure a -> Right a
  Nest n -> case n of
    Abort t -> Left t
    Emit _ _ next -> runPure next

runIO :: String -> Pipeline a -> IO a
runIO filename = \case
  Pure a -> pure a
  Nest n -> case n of
    Abort t -> do
      TextIO.putStrLn t
      exitFailure
    Emit ext content next -> do
      TextIO.writeFile (replaceExtensions filename ext) content
      runIO filename next
