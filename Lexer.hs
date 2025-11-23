module Lexer where 

import Data.Char 

data Token = TokenNum Int 
           | TokenTrue 
           | TokenFalse
           | TokenPlus 
           | TokenTimes 
           | TokenAnd 
           | TokenOr 
           | TokenLParen 
           | TokenRParen 
           | TokenIf          -- Adicionado
           | TokenThen        -- Adicionado
           | TokenElse        -- Adicionado
           | TokenVar String  -- Adicionado para variáveis
           | TokenLam         -- Adicionado (\)
           | TokenArrow       -- Adicionado (->)
           | TokenColon       -- Adicionado (:)
           | TokenBoolean     -- Adicionado (Tipo Bool)
           | TokenNumber      -- Adicionado (Tipo Num)
           deriving Show

-- Mantive a definição de Expr e Ty como estavam, pois estão corretas
data Expr = Num Int
          | BTrue
          | BFalse
          | Add Expr Expr
          | Times Expr Expr
          | And Expr Expr
          | Or Expr Expr
          | Paren Expr
          | If Expr Expr Expr
          | Var String
          | Lam String Ty Expr
          | App Expr Expr
          deriving Show

data Ty = TNum
        | TBool
        | TFun Ty Ty
        deriving (Show, Eq)

lexer :: String -> [Token]
lexer [] = []
lexer ('+':cs) = TokenPlus : lexer cs
lexer ('*':cs) = TokenTimes : lexer cs
lexer ('(':cs) = TokenLParen : lexer cs
lexer (')':cs) = TokenRParen : lexer cs
lexer ('&':'&':cs) = TokenAnd : lexer cs
lexer ('|':'|':cs) = TokenOr : lexer cs
lexer (':':cs) = TokenColon : lexer cs           -- Lê :
lexer ('\\':cs) = TokenLam : lexer cs            -- Lê \
lexer ('-':'>':cs) = TokenArrow : lexer cs       -- Lê ->
lexer (c:cs) | isSpace c = lexer cs
             | isDigit c = lexNum (c:cs)
             | isAlpha c = lexKw (c:cs)
lexer _ = error "Lexical error"

lexNum cs = case span isDigit cs of
              (num, rest) -> TokenNum (read num) : lexer rest

lexKw cs = case span isAlpha cs of
             ("true", rest)  -> TokenTrue : lexer rest
             ("false", rest) -> TokenFalse : lexer rest
             ("if", rest)    -> TokenIf : lexer rest
             ("then", rest)  -> TokenThen : lexer rest
             ("else", rest)  -> TokenElse : lexer rest
             ("Bool", rest)  -> TokenBoolean : lexer rest
             ("Num", rest)   -> TokenNumber : lexer rest
             (var, rest)     -> TokenVar var : lexer rest -- Lê variáveis genéricas