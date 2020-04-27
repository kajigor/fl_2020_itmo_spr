module FuncProgramsSidorova where


import Combinators (runParser)
import LLang (parseProg, parseDef)
import Data.List (intercalate)


recursiveFibMain = intercalate "\n" code where
  code = ["def main(arg) {",
          "    read (n);",
          "    write (fibonacci(n));",
          "};",
          "",
          "",
          "def fibonacci(n) {",
          "    if (n<=1) {",
          "        return n;",
          "    }",
          "    else {",
          "        return fibonacci(n-1) +",
          "               fibonacci(n-2);",
          "    };",
          "};"]


recursiveFac = intercalate "\n" code where
  code = ["def factorial(n) {",
          "    if (n<=1) {",
          "        return 1;",
          "    } else {",
          "        return n*factorial(n);",
          "    };",
          "};"]

recursiveDoubleFac = intercalate "\n" code where
  code = ["def doubleFactorial(n) {",
          "    if (n<=1) {",
          "        return 1;",
          "    }",
          "    else {",
          "        return n*doubleFactorial(n-2);",
          "    };",
          "};"]

factorialsMain = intercalate "\n" code where
  code = ["def main(arg) {",
          "    read (n);",
          "    write (factorial(n));",
          "    write (doubleFactorial(n));",
          "};",
          recursiveFac,
          recursiveDoubleFac]

power = intercalate "\n" code where
  code = ["def power(n, p) {",
          "    return n^p;",
          "};"]

powerSeq = intercalate "\n" code where
  code = ["def powerSeq(n, p) {",
          "    i = 0;",
          "    while (i<=n) {",
          "        write(power(i, n));",
          "    };",
          "};"]

powerSeqMain = intercalate "\n" code where
  code = ["def main(arg) {",
          "    read (n);",
          "    read (p);",
          "    _ = powerSeq(n,p);",
          "};",
          power,
          powerSeq]

