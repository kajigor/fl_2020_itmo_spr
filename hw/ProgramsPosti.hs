module Programs where

import Combinators (runParser, Result(..))
import LLang       (Configuration(..), parseL, eval)
import Data.List   (intercalate)
import qualified Data.Map as Map


fromSuccess (Success _ r) = r


doubleFactorial = intercalate "\n" code where
  code = [">> n",
          "let answer = 1",
          "let evenIter = 1",
          "while n > 1",
          "do",
          "    if evenIter",
          "    then",
          "        let answer = n * answer",
          "    else",
          "        let answer = answer",
          "    end",
          "    let evenIter = ! evenIter",
          "    let n = n - 1",
          "done",
          "<< answer\n"]
doubleFactorialExec n = eval (fromSuccess $ runParser parseL doubleFactorial) (Conf Map.empty [n] [])


powerSeq = intercalate "\n" code where
  code = [">> p",
          ">> n",
          "let i = 1",
          "while i <= n",
          "do",
          "    << i ^ p",
          "    let i = i + 1",
          "done\n"]
powerSeqExec p n = eval (fromSuccess $ runParser parseL powerSeq) (Conf Map.empty [p, n] [])


numAsDigits = intercalate "\n" code where
  code = [">> num",
          "if num < 0",
          "then",
          "    let num = -num",
          "else",
          "    let num = num",
          "end",
          "while num /= 0",
          "do",
          "    << num - 10 * (num / 10)",
          "    let num = num / 10",
          "done\n"]
numAsDigitsExec num = eval (fromSuccess $ runParser parseL numAsDigits) (Conf Map.empty [num] [])



