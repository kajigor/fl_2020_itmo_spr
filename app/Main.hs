import System.Environment (getArgs)
import System.IO (readFile)
import PParser (runParser)

main = do
    args <- getArgs
    contents <- readFile (head args)
    runParser contents

