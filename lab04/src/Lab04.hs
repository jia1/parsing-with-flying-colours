module Lab04 where

import           Control.Monad.State
import           Data.List                              as List
import           Text.Parsec                            hiding (State)
import           Text.Parsec.String                     (Parser)
import           Text.ParserCombinators.Parsec.Language (emptyDef)
import qualified Text.ParserCombinators.Parsec.Token    as Token

--
-- Parser section
--
data Term
  = Var String
  | Fun String Term
  | FApp Term Term
  | Let String Term Term
  deriving (Show, Eq)

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser lang
  where
    lang =
      emptyDef
        { Token.reservedOpNames = [".", "\\", "="]
        , Token.reservedNames = ["let", "in"]
        , Token.identStart = letter
        , Token.identLetter = letter
        }

parens :: Parser a -> Parser a
parens = Token.parens lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

ident :: Parser String
ident = Token.identifier lexer

var :: Parser Term
var = Var <$> ident

-- An example of a parser for lambdas with a single argument
-- You need to generalise it to lambdas with multiple arguments
function :: Parser Term
function = do
  reservedOp "\\"
  v <- ident
  reservedOp "."
  e <- expr
  return $ Fun v e

-- Code below needs to be considerably generalized
expr :: Parser Term
expr = function <|> var

parseAll :: Parser a -> String -> Either ParseError a
parseAll p =
  parse (allOf p) ""
  where
    allOf p = do
      Token.whiteSpace lexer
      r <- p
      eof
      return r

parseLambda :: String -> Maybe Term
parseLambda s
  = case of (parseAll s)
    Right parsed -> Just parsed
    Left _ -> Nothing


--
-- Evaluator section
--
freeVars :: Term -> [String]
freeVars t =
    let fvList =
            case t of
            Var s -> [s]
            Fun s t' -> freeVars t'
            FApp t1 t2 -> union (freeVars t1) (freeVars t2)
            Let s t1 t2 -> union [s] (union (freeVars t1) (freeVars t2))
    in fvList

alphaRename :: String -> String -> Term -> Term
alphaRename x x' t = sub x x' (freeVars t) t
  where
    sub x x' fvs t =
      let subst = sub x x' fvs in
      case t of
        Var s      -> if s == x && x' `notElem` fvs then Var x' else t
        Fun s t'   -> if s == x then Fun x' (subst t') else t
        FApp t1 t2 -> FApp (subst t1) (subst t2)
        Let s t1 t2 -> if s == x then Let x' t1 (subst t2) else Let s (subst t1) (subst t2)

-- alphaRename x f (\x . x)         --> \f . f
-- alphaRename x f (\f . f x)       --> \x . f x
-- alphaRename x f (\g . g(\x . x)) --> \g . g(\f . f)
-- alphaRename x f (\x . f x)       --> \x . f x

-- given an Int state, it generates a fresh string each time
freshNameGenerator :: State Int String
freshNameGenerator = do
  i <- get
  put (i + 1)
  return $ "_var" ++ show i

-- one may use a state monad from  Control.Monad.State, as below:
-- evalByName t = evalState (eval t) 0

betaReduce :: Term -> Term
betaReduce t = error "TBI(betaReduce)"

etaReduce :: Term -> Term
etaReduce t@(Fun x (FApp f (Var y))) = if x == y && x `notElem` freeVars f then f else t
etaReduce t = t

evalByName :: Term -> Term
evalByName t = error "TBI(evalByName)"

evalByValue :: Term -> Term
evalByValue t = error "TBI(evalByValue)"
