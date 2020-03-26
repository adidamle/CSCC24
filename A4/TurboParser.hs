module TurboParser where

import Control.Applicative

import ParserLib
import TurboDef

-- This can help testing by reading from a file so you can test multi-line input
-- and also have little hassle with \
parseFile :: String -> IO (Maybe Stmt)
parseFile filename = do
    inp <- readFile filename
    let ans = runParser mainParser inp
    return ans

mainParser :: Parser Stmt
mainParser = whitespaces *> stmt <* eof

--------------------------------------- THIS SECTION IS FOR --------------------------------------
--------------------------------------- PARSING THE STATEMENTS -----------------------------------
stmt :: Parser Stmt
stmt = assign <|> penCmd <|> turn <|> forward <|> forLoop <|> mysequence

-- This method parses the String := RealExpr orthe assign operation of the stmt.
assign :: Parser Stmt
assign = do
    vName <- kwords
    terminal "="
    e <- expr
    return (vName := e)

-- This method parses both the pen commands of the stmt.
penCmd :: Parser Stmt
penCmd = (do keyword "pendown"
             return PenDown)
          <|>
         (do keyword "penup"
             return PenUp)

-- This method parses the turn of the stmt.
turn :: Parser Stmt
turn = do
    keyword "turn"
    myexpr <- expr
    return (Turn myexpr)

-- This method parses the forward of the stmt.
forward :: Parser Stmt
forward = do
    keyword "forward"
    myexpr <- expr
    return (Forward myexpr)


-- This method parses the for loop of the stmt.
-- It makes use of the helper function getStmtList to
-- parse the list and adds it as a parameter for the loop.
forLoop :: Parser Stmt
forLoop = do
    keyword "for"
    vName <- kwords
    terminal "="
    initialExpr <- expr
    keyword "to"
    endExpr <- expr
    stmtList <- getStmtList
    return (For vName initialExpr endExpr stmtList)


-- This method parses the Seq of the stmt.
-- It makes use of the helper function getStmtList to
-- parse the list and adds it as a parameter for the loop.
mysequence :: Parser Stmt
mysequence = do 
    stmtList <- getStmtList
    return (Seq stmtList)


-- This helper parses the sequence of stmts and returns the list
-- of the statements.
getStmtList :: Parser [Stmt]
getStmtList = do
    terminal "{"
    stmtList <- many singleStatement
    terminal "}"
    return (stmtList)


-- This helper parses a single statement between the {} for the seq
-- and also checks if each stmt is followed with a ';' seperator.
singleStatement :: Parser Stmt
singleStatement = do
    s <- stmt
    terminal ";"
    return s

--------------------------------------- THIS SECTION IS FOR --------------------------------------
--------------------------------------- PARSING THE EXPRESSION -----------------------------------
expr :: Parser RealExpr
expr = adds <|> atom

atom :: Parser RealExpr
atom = negateExpr <|> brackets <|> literal <|> var

---------------------------------------------------
-- This function parses the literals.
-- It parses decimals as well as non decimals.
literal :: Parser RealExpr
literal = fmap (\x -> (RLit x)) parseDecimal <|> fmap (\x -> (RLit (fromIntegral x))) integer

-- This helper function parses the decimal number and returns the number.
parseDecimal :: Parser Double
parseDecimal = do
    beforeD <- natural
    decimalDot <- char '.'
    afterD <- natural
    let dec = (show beforeD) ++ "." ++ (show afterD)
    let decD = read dec :: Double
    return decD

---------------------------------------------------

-- THIS SECTION PARSES THE VARIABLES, EXPRESSIONS IN THE BRACKETS AND 
-- THE NEGATED EXPRESSIONS.
var ::Parser RealExpr
var = do
    varName <- kwords
    return (RVar varName)

brackets ::Parser RealExpr
brackets = terminal "(" *> expr <* terminal ")"

negateExpr ::Parser RealExpr
negateExpr = do
    terminal "-"
    e <- expr
    return (Neg e)

---------------------------------------------------
-- The adds and muls funtions do the operation chaining by 
-- calling the chainl1 method for the operations are left-assosiated.
-- These methods also consider the order of precedence for the operations.
adds :: Parser RealExpr
adds = chainl1 muls addop

muls :: Parser RealExpr
muls = chainl1 atom mulop

-- This is a helper to handle addition and subtraction operations
-- and returns the desired infix operator.
addop :: Parser (RealExpr -> RealExpr -> RealExpr)
addop = (operator "+" *> pure (:+)) <|> (operator "-" *> pure (:-))

-- This is a helper to handle multiplication and division operations
-- and returns the desired infix operator.
mulop :: Parser (RealExpr -> RealExpr -> RealExpr)
mulop = (operator "*" *> pure (:*)) <|> (operator "/" *> pure (:/))

---------------------------------------------------

-- THIS IS THE LIST OF RESERVED KEYWORDS.
kwords :: Parser String
kwords = identifier ["pendown", "penup", "turn", "forward", "for", "to"]