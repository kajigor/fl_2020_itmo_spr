module FuncProgramsPosti where


import Parser (runParser)
import LLang (parseFunction, parseProgram)
import Data.List (intercalate)


recursiveFibMain = intercalate "\n" code where
  code = ["fun main:",
          "  >> n",
          "  << @{fibonacci n}",
          "",
          "fun fibonacci n:",
          "    if n <= 1",
          "    then",
          "        return n",
          "    else",
          "        return @{fibonacci n - 1} + @{fibonacci n - 2}",
          "    end",
          ""]

doubleFactorial = intercalate "\n" code where
  code = ["fun doubleFactorial n:",
          "    if n <= 1",
          "    then",
          "        return n",
          "    end",
          "    return n * @{doubleFactorial n - 2}",
          ""]


factorial = intercalate "\n" code where
  code = ["fun factorial n:",
          "   if n <= 1",
          "   then",
          "       return n",
          "   end",
          "   return n * @{factorial n}",
          ""]

factorialsMain = intercalate "\n" code where
  code = ["fun main:",
          "  >> n",
          "  << @{factorial n}",
          "  << @{doubleFactorial n}",
          factorial,
          doubleFactorial]

power = intercalate "\n" code where
  code = ["fun power n, p:",
          "    return n ^ p",
          ""]

powerSeq = intercalate "\n" code where
  code = ["fun powerSeq n, p:",
          "  let i = 0",
          "  while i <= n",
          "  do",
          "    << @{power i, p}",
          "    let i = i + 1",
          "  done",
          ""]

powerSeqMain = intercalate "\n" code where
  code = [power,
          powerSeq,
          "fun main:",
          "  >> n",
          "  >> p",
          "  let _ = @{powerSeq n,p}",
          ""]
