module Lab12 where

import Control.Applicative
import ParserLib

import Lab12Def

{-
Lisp and Scheme use S-expressions. A basic version of the grammar in EBNF is:

    S ::= identifier
        | "(" S { S } ")"

identifier is as in ParserLib.hs for simplicity.  We are also omitting various
literals like numbers and quotations.

Whitespaces can surround identifiers, "(", and ")".  For example this input is
legal:

    "   (  f  ( g  x1 y1)  (h))  "

and is parsed to

    List [Ident "f", List [Ident "g",Ident "x1",Ident "y1"], List [Ident "h"]]

Implement a parser for this:

* mainParser is responsible for skipping leading whitespaces, calling sexpr
  once, and checking the lack of leftovers after.  Think of this as the
  user-facing entry point, e.g., a user would use "runParser mainParser".

  runParser mainParser "   ( f x1 )   " = Just (List [Ident "f", Ident "x1"])
  runParser mainParser "   ( f x1 ) y " = Nothing

* sexpr assumes there is no leading whitespaces (because mainParser already took
  care of that).  It is responsible for skipping trailing whitespaces.  It does
  not check for lack of leftovers; it just consumes and parses what it needs and
  leaves the rest unconsumed.  Think of this as a helper parser.

  dePsr sexpr "( f x1 )   " = Just ("",   List [Ident "f", Ident "x1"])
  dePsr sexpr "( f x1 ) y " = Just ("y ", List [Ident "f", Ident "x1"])

8 marks for sexpr, and 2 marks for mainParser.
-}

sexpr :: Parser SExpr
sexpr = fmap (\string -> Ident string) (identifier[]) <* whitespaces <|>
 fmap (\sexpr -> List sexpr) (satisfy (=='(') *> whitespaces *> some sexpr <* whitespaces <* satisfy (==')')) <* whitespaces

mainParser :: Parser SExpr
mainParser = whitespaces *> sexpr