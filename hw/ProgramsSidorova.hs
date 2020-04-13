module Programs where

import Combinators (runParser)
import LLang       (parseL)
import Data.List   (intercalate)

doubleFactorial = intercalate "" code where
  code = ["read (n);",
          "answer = 1;",
          "evenIter = 1;",
          "while (n>1) {",
          "    if (evenIter) {",
          "        answer = n*answer;",
          "    }",
          "    else {",
          "    };",
          "    evenIter = !evenIter;",
          "    n = n-1;",
          "};",
          "write (answer);"]


powerSeq = intercalate "" code where
  code = ["read (p);",
          "read (n);",
          "i = 1;",
          "while (i<=n) {",
          "    write (i^p);",
          "    i = i+1;",
          "};"]


numAsDigits = intercalate "" code where
  code = ["read (num);",
          "if (num<0) {num = -1*num;} else {};",
          "while (num/=0) {",
          "    write (num-10*(num/10));",
          "    num = num/10;",
          "};"]



