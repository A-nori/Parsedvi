import System.Environment (getArgs)
import qualified System.IO as IO
import qualified Data.ByteString as B
import Command
import Dump


main = do
  args <- getArgs
  if null args then do
    putStrLn "too short argument"
  else do
    inh <- IO.openBinaryFile (head args) IO.ReadMode
    input <- B.unpack <$> B.hGetContents inh
    let 
      input' = Command.convert input
    Dump.dump_list input' 0
