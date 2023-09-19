module Parser where

import           Text.ParserCombinators.Parsec
import           Text.Parsec.Token
import           Text.Parsec.Language           ( emptyDef )
import           Text.Parsec.String
import           Text.Parsec.Prim
import           Text.Parsec.Combinator
import           AST

-----------------------
-- Funcion para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do
  whiteSpace lis
  t <- p
  eof
  return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser
  (emptyDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , opLetter        = char '='
    , reservedNames   = ["true", "false", "if", "else", "repeat", "skip", "until"]
    , reservedOpNames = [ "+"
                        , "-"
                        , "*"
                        , "/"
                        , "<"
                        , ">"
                        , "&&"
                        , "||"
                        , "!"
                        , "="
                        , "=="
                        , "!="
                        , ";"
                        , ","
                        ]
    }
  )

----------------------------------
--- Parser de expressiones enteras
-----------------------------------
-- parsea: term +- term +- ...
intexp :: Parser (Exp Int) 
intexp = chainl1 intterm opPlusMin

-- parsea: factor */ factor */ ...
intterm = chainl1 intfactor opTimesDiv 

-- parsea: var
--         int
--         -...
intfactor = do var <- identifier lis
               return $ Var var
         <|> do int <- integer lis
                return $ Const int
         <|> do reservedOp lis "-"
                f <- intfactor
                return $ UMinus f

opPlusMin = do reservedOp lis "+"
               return $ Plus
            <|> do reservedOp lis "-"
                   return $ Minus


opTimesDiv = do reservedOp lis "*"
                return Times
             <|> do reservedOp lis "/"
                    return Div

-----------------------------------
--- Parser de expressiones booleanas
------------------------------------
-- parsea: boolterm || boolterm || ...
boolexp :: Parser (Exp Bool)
boolexp = chainl1 boolterm opOr

-- parsea: boolfactor && boolfactor && ...
boolterm = chainl1 boolfactor opAnd

-- parsea: !...
--         ... </>/!=/== ...
--         true | false
boolfactor = do reservedOp lis "!" -- Not ...
                f <- boolfactor
                return $ Not f
             <|> do int <- intexp -- (== != < >)
                    opC <- opCompare
                    int <- intexp
                    return $ opC int int
             <|> do v <- bVal -- true |false
                    return v

opOr = do reservedOp lis "||"
          return Or

opAnd = do reservedOp lis "&&"
           return And

opCompare = do reservedOp lis "<"
               return Lt
            <|> do reservedOp lis ">"
                   return Gt
            <|> do reservedOp lis "=="
                   return Eq
            <|> do reservedOp lis "!="
                   return NEq

bVal = do reserved lis "true"
          return $ BTrue
       <|> do reserved lis "false"
              return $ BTrue




-----------------------------------
--- Parser de comandos
-----------------------------------

-- parsea commterm ; commterm ; ...
comm :: Parser Comm
comm = chainl1 commterm opcomm

-- parsea: var = intexp
--         ifThen
--         ifThenELse
--         repeatUntil
commterm = do v <- identifier lis
              reserved lis "="
              n <- intexp
              return $ Let v n
           <|> do commt <- ifThenElse
                  return commt
           <|> do commt <- repeatUntil
                  return commt

opcomm = do reservedOp lis ";"
            return Seq

-- parsea: repeat {...} until (...)
repeatUntil = do reserved lis "repeat"
                 c <- braces lis comm -- entre {}
                 reserved lis "until"
                 b <- parens lis boolexp -- entre ()
                 return $ Repeat c b

-- parsea: if ... then {...} else {...}
ifThenElse = do reserved lis "if"
                bool <- boolexp
                c1 <- braces lis comm
                (do reserved lis "else"
                    c2 <- braces lis comm -- entre {}
                    return $ IfThenElse bool c1 c2
                 <|> return (IfThen bool c1))

------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
