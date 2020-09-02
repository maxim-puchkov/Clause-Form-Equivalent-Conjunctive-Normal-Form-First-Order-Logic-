module FormulaTokenizer where
import Data.Char

type Identifier = [Char]

data Token =  Not | And | Or |
              Impl | Equ | ExisQ |
              UnivQ |
              Id Identifier |
              Variable Identifier |
              Lparen | Rparen | Comma |
              BadToken Char
              deriving Show

tokenize [] = []
tokenize (' ':more) = tokenize more            -- skip white space
tokenize ('\n':more) = tokenize more           -- skip white space
tokenize ('(':more) = Lparen: (tokenize more)
tokenize (')':more) = Rparen: (tokenize more)
tokenize (',':more) = Comma: (tokenize more)
tokenize t@(c:more)
   | isAlpha c = getName [c] (span isAlphaNum more)
   | otherwise = BadToken c : tokenize (more)

getName pfx (alphanum, s@('-':c:more))
  | isAlphaNum c   = getName (pfx ++ alphanum ++ ['-',c]) (span isAlphaNum more)
  | otherwise      = nameOf (pfx ++ alphanum) : (tokenize s)
getName pfx (alphanum, more) = nameOf (pfx ++ alphanum) : (tokenize more)

nameOf :: [Char] -> Token
nameOf ("not") = Not
nameOf ("and") = And
nameOf ("or") = Or
nameOf ("implies") = Impl
nameOf ("equiv") = Equ
nameOf ("exists") = ExisQ
nameOf ("forall") = UnivQ
nameOf s
  | isUpper (head s) = Variable s
  | isLower (head s) = Id s
  | otherwise = BadToken (head s)
