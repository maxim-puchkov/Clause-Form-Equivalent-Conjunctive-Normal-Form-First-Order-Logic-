module ClauseFormEquivalent where

import Data.Char
import qualified Data.Map.Strict as Env
import Formula

type ClauseFormEquivalent = [Clause]
type Clause = [Literal]
data Literal = Pos Name [Term] | Neg Name [Term] deriving Show

convertToClauseFormEquivalent :: Formula -> ClauseFormEquivalent
convertToClauseFormEquivalent f = clauses (conjunctiveNormalForm f)





-- 7. Translate CNF to Clause-Form Equivalent
clauses (Conj fs) = map (clause) fs
clauses f = []

clause (Disj fs) = map (literal) fs
clause f = [literal f]

literal f@(Predicate n t) = Pos n t
literal (Negation f) = Neg (fst lit) (snd lit)
  where lit = (litNT f)
litNT (Predicate n t) = (n, t)
litNT (Negation f) = litNT f

countPosLitClauses cs = length [cl | cl <- cs, not (null (onlyPosLit cl))]

onlyPosLit c = [lit | lit <- c, (isPos lit)]
isPos (Pos _ _) = True
isPos (Neg _ _) = False
