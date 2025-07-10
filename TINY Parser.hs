-- V1: TINY Parser, built up from the expression parser example from section 8.8 of Programming in Haskell, Graham Hutton, Cambridge University Press, 2007.
-- V2: SMALL Parser, built using the standard in Chapter 6 of Michael J. C. Gordon's 'THE DENOTATIONAL DESCRIPTION OF PROGRAMMING LANGUAGES'
-- Developed by Aaron Win

module TINYParser where

import Parsing

type Ide = String

-- V2:

type Bas = Int

data Opr = Add | Subtract | Multiply | Divide | GreaterThan | LesserThan
           deriving Show

opr :: Parser Opr
opr = do symbol "+"
         return Add
      +++
      do symbol "-"
         return Subtract
      +++
      do symbol "*"
         return Multiply
      +++
      do symbol "/"
         return Divide
      +++
      do symbol ">"
         return GreaterThan
      +++
      do symbol "<"
         return LesserThan

-- V1:

--data Exp = Zero | One | TT | FF | Read | I Ide | Not Exp | Equal Exp Exp | Plus Exp Exp
--           deriving Show

-- V2:

data Exp = B Bas | TT | FF | Read | I Ide | FunE Exp Exp | IfThenElseE Exp Exp Exp | Operate Opr Exp Exp
           deriving Show

-- V1:

--expr :: Parser Exp
--expr = do t <- term
--          do symbol "+"
--             e <- expr
--             return (Plus t e)
--       +++
--       do t <- term
--          do symbol "="
--             e <- expr
--             return (Equal t e)
--       +++
--       do symbol "("
--          e <- expr
--          do symbol ")"
--             return e
--       +++
--       term

--term :: Parser Exp
--term = do symbol "not"
--          e <- expr
--          return (Not e)
--       +++
--       factor

--factor :: Parser Exp
--factor = do symbol "read"
--            return Read
--         +++
--         do symbol "false"
--            return FF
--         +++
--         do symbol "true"
--            return TT
--         +++
--         do symbol "0"
--            return Zero
--         +++
--         do symbol "1"
--            return One
--         +++
--         do i <- identifier
--            return (I i)

-- V2:

expr :: Parser Exp
expr = do e1 <- factor
          do symbol "("
             e2 <- expr
             do symbol ")"
                return (FunE e1 e2)
       +++
       do symbol "if"
          e <- expr
          do symbol "then"
             e1 <- expr
             do symbol "else"
                e2 <- expr
                return (IfThenElseE e e1 e2)
       +++
       do e1 <- factor
          o <- opr
          e2 <- expr
          return (Operate o e1 e2)
       +++
       do symbol "("
          e <- expr
          do symbol ")"
             return e
       +++
       factor

factor :: Parser Exp
factor = do b <- nat
            return (B b)
         +++
         do symbol "false"
            return FF
         +++
         do symbol "true"
            return TT
         +++
         do symbol "read"
            return Read
         +++
         do i <- identifier
            return (I i)

-- V1:

--data Cmd = Assign Ide Exp | Output Exp | IfThenElse Exp Cmd Cmd | WhileDo Exp Cmd | Seq Cmd Cmd
--           deriving Show

-- V2:

data Cmd = Assign Exp Exp | Output Exp | ProcC Exp Exp | IfThenElseC Exp Cmd Cmd | WhileDo Exp Cmd | BeginEnd Dec Cmd | SeqD Cmd Cmd
           deriving Show

cmd :: Parser Cmd
cmd = do c1 <- com
         do symbol ";"
            c2 <- cmd
            return (SeqD c1 c2)
      +++
      do symbol "("
         c <- cmd
         do symbol ")"
            return c
      +++
      com

-- V1:

--com :: Parser Cmd
--com = do i <- ident [" "]
--         do symbol ":="
--            e <- expr
--            return (Assign i e)
--      +++
--      do symbol "output"
--         e <- expr
--         return (Output e)
--      +++
--      do symbol "if"
--         e <- expr
--         do symbol "then"
--            c1 <- cmd
--            do symbol "else"
--               c2 <- cmd
--               return (IfThenElse e c1 c2)
--      +++
--      do symbol "while"
--         e <- expr
--         do symbol "do"
--            c <- cmd
--            return (WhileDo e c)

-- V2:

com :: Parser Cmd
com = do e1 <- expr
         do symbol ":="
            e2 <- expr
            return (Assign e1 e2)
      +++
      do symbol "output"
         e <- expr
         return (Output e)
      +++
      do e <- expr
         do symbol "("
            e1 <- expr
            do symbol ")"
               return (ProcC e e1)
      +++
      do symbol "if"
         e <- expr
         do symbol "then"
            c1 <- cmd
            do symbol "else"
               c2 <- cmd
               return (IfThenElseC e c1 c2)
      +++
      do symbol "while"
         e <- expr
         do symbol "do"
            c <- cmd
            return (WhileDo e c)
      +++
      do symbol "begin"
         d <- dec
         do symbol ";"
            c <- cmd
            do symbol "end"
               return (BeginEnd d c)

cparse :: String -> Cmd
cparse xs = case (parse cmd xs) of
              [(n,[])] -> n
              [(_,out)] -> error ("unused input " ++ out)
              [] -> error "invalid input"

data Dec = Const Ide Exp | Var Ide Exp | ProcD Ide Ide Cmd | FunD Ide Ide Exp | SeqD Dec Dec
           deriving Show

dec :: Parser Dec
dec = do d1 <- decm
         do symbol ";"
            d2 <- dec
            return (SeqD d1 d2)
      +++
      do symbol "("
         d <- dec
         do symbol ")"
            return d
      +++
      decm

decm :: Parser Dec
decm = do symbol "const"
          i <- ident [" "]
          do symbol "="
             e <- expr
             return (Const i e)
       +++
       do symbol "var"
          i <- ident [" "]
          do symbol "="
             e <- expr
             return (Var i e)
       +++
       do symbol "proc"
          i <- ident [" "]
          do symbol "("
             i1 <- ident [" "]
             do symbol ")"
                do symbol ";"
                   c <- cmd
                   return (ProcD i i1 c)
       +++
       do symbol "fun"
          i <- ident [" "]
          do symbol "("
             i1 <- ident [" "]
             do symbol ")"
                do symbol ";"
                   e <- expr
                   return (FunD i i1 e)

data Pro = Program Cmd

pro :: Parser Pro
pro = do symbol "program"
         c <- cmd
         return (Program c)