module PA1Helper(runProgram,Lexp(..)) where

import System.Directory
import System.Environment
import Control.Monad
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Text.Parsec.Char

-- Haskell representation of lambda expression
data Lexp = Atom String | Lambda String Lexp | Apply Lexp Lexp deriving Eq

-- Allow for Lexp datatype to be printed like the Oz representation of a lambda expression
instance Show Lexp  where 
    show (Atom v) = v
    show (Lambda exp1 exp2) = "\\" ++ exp1 ++ "." ++ (show exp2) 
    show (Apply exp1 exp2) = "(" ++ (show exp1) ++ " " ++ (show exp2) ++ ")" 


-- Reserved keywords in Oz
-- P. 841 Table C.8, "Concepts, Techniques, and Models of Computer Programming", 
-- Van Roy, Haridi
ozKeywords = ["andthen","at","attr","break"
              ,"case","catch","choice","class"
              ,"collect","cond","continue"
              ,"declare","default","define"
              ,"dis","div","do","else"
              ,"elsecase","elseif","elseof"
              ,"end","export","fail","false"
              ,"feat","finally","for","from"
              ,"fun","functor","if","import"
              ,"in","lazy","local","lock"
              ,"meth","mod","not","of","or"
              ,"orelse","prepare","proc"
              ,"prop","raise","require"
              ,"return","self","skip","then"
              ,"thread","true","try","unit"
              ] 

-- Sparse language definition to define a proper Oz identifier
-- An atom is defined as follows:
-- 1. sequence of alphanumeric chars starting with a lowercase letter, 
--   excluding language keywords
-- 2. arbitrary printable chars enclosed in single quotes, 
--   excluding "'", "\", and "NUL"
-- lDef defines an atom as only 1.
-- P. 825,"Concepts, Techniques, and Models of Computer Programming", 
-- Van Roy, Haridi
lDef = emptyDef { identStart = lower
                , identLetter = alphaNum
                , reservedNames = ozKeywords
                } 
-- Obtains helper functions for parsing 
TokenParser{ parens = m_parens
           , identifier = m_identifier
           , reserved = m_reserved
           , brackets   = m_brackets
           } = makeTokenParser lDef

-- Below is code to parse Oz lambda expressions and represent them in Haskell
atom = do
  var <- m_identifier
  return (Atom var)

lambargs = do
  var1 <- m_identifier
  char '.'
  var2 <- start
  return (Lambda var1 var2)

lamb = do
  char '\\'
  p <- lambargs
  return p

appargs = do
    var1 <- start
    spaces
    var2 <- start
    return (Apply var1 var2)

app = do
  p <- m_parens appargs 
  return p

start = atom <|> lamb <|> app

-- Use previously defined parser to parse a given String
parseLExpr :: String -> Either ParseError Lexp 
parseLExpr input = parse start "" input

-- Gracefully handle parse errors, proceeding to the next expression in the file and printing a helpful message
handler :: (Lexp -> Lexp) -> String -> Int -> String -> IO ()
handler reducer outFile n str  = case parseLExpr str of
    Left err -> do
      putStrLn ("Parse error for expression " ++ show n ++ ": " ++ show err)
      writeFile outFile "Error"
    Right lexp -> outputPrinter outFile n lexp (reducer lexp)

doDebugPrint = True

-- Pretty printer for outputting inputted lambda expressions along with
-- their reduced expressions. Integer used to distiguish between test cases.
-- Note - the stdout printing is for your convenience only; we will be
-- grading the contents of the output file
outputPrinter :: String -> Int -> Lexp -> Lexp -> IO ()
outputPrinter outFile n lexp lexp' = do
    appendFile outFile ((show lexp') ++ "\n")
    when doDebugPrint $ do
      putStrLn ("Input  " ++ (show n) ++ ": " ++ (show lexp))
      putStrLn ("Result " ++ (show n) ++ ": " ++ (show lexp'))
      putStrLn ""

-- Given input/output f iles and a function for reducing lambda expressions,
-- reduce all valid lambda expressions in the file and output results.
runProgram :: String -> String -> (Lexp -> Lexp) -> IO ()
runProgram inFile outFile reducer = do
    exists <- doesFileExist outFile
    when exists $ removeFile outFile
    fcontents <- readFile inFile
    let inList = lines fcontents
    sequence_ (zipWith (handler reducer outFile) [1..] inList)

