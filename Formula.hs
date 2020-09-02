module Formula where

import Data.Char
import Data.List

data Formula = Predicate Name [Term] | Negation Formula
               | Conj [Formula] | Disj [Formula]
               | Implies Formula Formula | Equiv Formula Formula
               | ForAll Variable Formula | Exists Variable Formula
               deriving Show

data Term = Function Name [Term] | Var [Char] deriving Show
type Name = [Char]
type Variable = [Char]

conjunctiveNormalForm :: Formula -> Formula
conjunctiveNormalForm f =
  flatter (
    eliminateUniversalQuantifier (
      eliminateExistentialQuantifier (
        standardizedEnvironment f,
        standardize (
          propagate (
            eliminateIE f
          )
        )
      )
    )
  )






-- 1. Eliminate Implications and Equalities
eliminateIE :: Formula -> Formula
eliminateIE f@(Predicate n terms) = f
eliminateIE (Negation f) = (Negation (eliminateIE f))
eliminateIE (Conj formulas) = Conj (map eliminateIE formulas)
eliminateIE (Disj formulas) = Disj (map eliminateIE formulas)
eliminateIE (ForAll v f) = ForAll v (eliminateIE f)
eliminateIE (Exists v f) = Exists v (eliminateIE f)
eliminateIE (Implies f1 f2) = Disj [Negation (eliminateIE f1), eliminateIE f2]
eliminateIE (Equiv f1 f2) =
   let (e1, e2) = (eliminateIE f1, eliminateIE f2)
       in Conj [Disj [Negation e1, e2], Disj [Negation e2, e1]]






-- 2. Inward propagation of negations
propagate :: Formula -> Formula
propagate f@(Predicate n terms) = f
propagate (Negation f) = negatef f
propagate (Conj formulas) = Conj (map propagate formulas)
propagate (Disj formulas) = Disj (map propagate formulas)
propagate (ForAll v f) = ForAll v (propagate f)
propagate (Exists v f) = Exists v (propagate f)

negatef :: Formula -> Formula
negatef f@(Predicate n terms) = (Negation f)
negatef (Negation f) = propagate f
negatef (Conj formulas) = Disj (map negatef formulas)
negatef (Disj formulas) = Conj (map negatef formulas)
negatef (ForAll v f) = Exists v (negatef f)
negatef (Exists v f) = ForAll v (negatef f)






-- 3. Variable Standardization - Variable Environment
type Association = (Variable, Variable)
type VariableEnv = [Association]

stdEnv :: Formula -> VariableEnv
stdEnv f = uniqueVarEnv f []

addAssoc :: VariableEnv -> Association -> VariableEnv
addAssoc env (name, uname) = env ++ [(name, uname)]

delAssoc :: VariableEnv -> Variable -> VariableEnv
delAssoc env v = case lookup v env of
  Just a -> [(n1, n2) | (n1, n2) <- env, not (n1 == v && n2 == a)]
  _ -> []

delFormAssoc e (Predicate n t) = delTermsAssoc e t
delFormAssoc e (Negation f) = delFormAssoc e f
delFormAssoc e (Conj []) = e
delFormAssoc e (Conj (f:fs)) = delFormAssoc (delFormAssoc e f) (Conj fs)
delFormAssoc e (Disj []) = e
delFormAssoc e (Disj (f:fs)) = delFormAssoc (delFormAssoc e f) (Disj fs)
delFormAssoc e (ForAll v f) = delFormAssoc e f

delTermsAssoc :: VariableEnv -> [Term] -> VariableEnv
delTermsAssoc env [] = env
delTermsAssoc env ((Function n t):ts) = delTermsAssoc (delTermsAssoc env t) ts
delTermsAssoc env ((Var v):ts) = delTermsAssoc (delAssoc env v) ts

termAssoc :: VariableEnv -> Variable -> Maybe Association
termAssoc env v
  | length t == 1   = Just (head t)
  | otherwise       = Nothing
  where t = [(a, b) | (a, b) <- env, v == b]

applyVarEnv :: VariableEnv -> Variable -> Variable
applyVarEnv env v =
    case lookup v env of
        Just a -> a
        _ -> "Error"


-- Variable Names
uniqueVarEnv :: Formula -> VariableEnv -> VariableEnv
uniqueVarEnv f env = makeUnique (vars f) [] env

makeUnique :: [Name] -> [Name] -> VariableEnv -> VariableEnv
makeUnique [] res env = env
makeUnique (v:vs) res env = makeUnique vs (res ++ [u]) (addAssoc env (v, u))
    where u = gensym env v

vars :: Formula -> [Variable]
vars (Predicate n terms) = termVars terms
vars (Negation f) = vars f
vars (Conj fs) = allVars fs
vars (Disj fs) = allVars fs
vars (Implies f1 f2) = vars f1 ++ vars f2
vars (ForAll v f) = [v] ++ vars f
vars (Exists v f) = [v] ++ vars f

allVars :: [Formula] -> [Variable]
allVars [] = []
allVars (f:fs) = vars f ++ allVars fs

termVars :: [Term] -> [Variable]
termVars [] = []
termVars ((Var v):ts) = [v] ++ termVars ts
termVars ((Function f t):ts) = termVars t ++ termVars ts


-- Generate Symbol Names
gensym :: VariableEnv -> Variable -> Variable
gensym env sym
  | n == 0            = sym
  | otherwise         = sym ++ show n
  where n = count sym env

count :: Variable -> VariableEnv -> Int
count sym env = length [x | (x, y) <- env, x == sym]


-- Standardize terms and functions
standardize :: Formula -> Formula
standardize f = stdf (standardizedEnvironment f) f

standardizedEnvironment :: Formula -> VariableEnv
standardizedEnvironment f = stdEnv f

stdTs :: VariableEnv -> [Term] -> [Term]
stdTs e [] = []
stdTs e ((Function n t):ts) = [Function n (stdTs e t)] ++ stdTs (delTermsAssoc e t) ts
stdTs e ((Var v):ts) = [stdt e (Var v)] ++ stdTs (delAssoc e v) ts

stdt :: VariableEnv -> Term -> Term
stdt e (Function n t) = Function n (stdTs e t)
stdt e (Var v) = Var (applyVarEnv e v)

stdFs :: VariableEnv -> [Formula] -> [Formula]
stdFs e [] = []
stdFs e (f@(Predicate n t):fs) = [stdf e f] ++ stdFs (delTermsAssoc e t) fs
stdFs e (f:fs) = [stdf e f] ++ stdFs e fs

stdf :: VariableEnv -> Formula -> Formula
stdf e (Predicate n t) = Predicate n (stdTs e t)
stdf e (Negation f) = Negation (stdf e f)
stdf e (Conj []) = Conj []
stdf e (Conj (f:fs)) = Conj ([stdf e f] ++ (stdFs (delFormAssoc e f) fs))
stdf e (Disj []) = Disj []
stdf e (Disj (f:fs)) = Disj ([stdf e f] ++ (stdFs (delFormAssoc e f) fs))
--stdf e (Disj fs) = Disj (stdFs e fs)
stdf e (ForAll v f) = ForAll (applyVarEnv e v) (stdf (delAssoc e v) f)
stdf e (Exists v f) = Exists (applyVarEnv e v) (stdf (delAssoc e v) f)







-- 4. Eliminate Existential Quantifiers
type QuantifierScope = [QuantifierAssociation]
type QuantifierAssociation = (UniversalVariable, [ExistentialVariable])
type ExistentialVariable = Variable
type UniversalVariable = Variable

eliminateExistentialQuantifier :: (VariableEnv, Formula) -> Formula
eliminateExistentialQuantifier (env, f) = eliminateEQ env [] [] f


addToScope :: ExistentialVariable -> ExistentialVariable -> QuantifierScope -> QuantifierScope -> QuantifierScope
addToScope v v2 [] res = res
addToScope v v2 (s@(evar, uvars):scopes) res
  | evar == v   = addToScope v v2 scopes (res ++ [(evar, uvars ++ [v2])])
  | otherwise = addToScope v v2 scopes (res ++ [s])

addToLastScope :: ExistentialVariable -> QuantifierScope -> QuantifierScope -> QuantifierScope
addToLastScope v [] res = res ++ [("const", [v])]
addToLastScope v ((u, vars):[]) res = res ++ [(u, vars ++ [v])]
addToLastScope v (s@(u, vars):scopes) res = addToLastScope v scopes (res ++ [s])

newScope :: UniversalVariable -> QuantifierScope -> QuantifierScope
newScope v s = s ++ [(v, [])]

eliminateEQ :: VariableEnv -> [ExistentialVariable] -> QuantifierScope -> Formula -> Formula
eliminateEQ [] [] [] f@(Predicate n t) = f
eliminateEQ env v s (Predicate n t) = (Predicate n (map (eliminateEQTerm env v s) t))
eliminateEQ env v s (Negation f) = Negation (eliminateEQ env v s f)
eliminateEQ env v s (Conj fs) = Conj (map (eliminateEQ env v s) fs)
eliminateEQ env v s (Disj fs) = Disj (map (eliminateEQ env v s) fs)
eliminateEQ env v s (ForAll v2 f) = ForAll v2 (eliminateEQ env v (newScope v2 s) f)
eliminateEQ env v s (Exists v2 f) = (eliminateEQ env (v ++ [v2]) (addToLastScope v2 s []) f)
eliminateEQ env v s f@(_) = eliminateEQ env v s f

eliminateEQTerm :: VariableEnv -> [Variable] -> QuantifierScope -> Term -> Term
eliminateEQTerm env v s (Var v2) = case (termAssoc env v2) of
  Just (a, b) ->
    if (elem a v) then skolem v a s
    else (Var v2)
  _ -> Var "err"
eliminateEQTerm env v s (Function n t) = Function n (map (eliminateEQTerm env v s) t)


-- Skolemization
skolem :: [Variable] -> Variable -> QuantifierScope -> Term
skolem vars v s = Function (genUniqueSkolemSym vars v) (skolemTerms vars v s)

_SkolemConstScope = "const"
_SkolemFunctionName = "sk"
_SkolemConstTerm = "c"

genUniqueSkolemSym :: [Variable] -> Variable -> Name
genUniqueSkolemSym vars v
  | i > 0       = _SkolemFunctionName ++ (show i)
  | i == 0      = _SkolemFunctionName
  | otherwise   = "skolem_error"
  where i = skolemId vars v

skolemId :: [Variable] -> Variable -> Int
skolemId vars v = case elemIndex v vars of
  Just i -> i
  _ -> -1

constScope ((u, e):scopes)
  | u == _SkolemConstScope   = Just (u, e)
  | otherwise                = Nothing

skolemConstTermId :: Variable -> QuantifierScope -> Int
skolemConstTermId v s = case (constScope s) of
  Just (u, e) -> case elemIndex v e of
    Just a -> a
  _ -> -1

skolemTerms :: [Variable] -> Variable -> QuantifierScope -> [Term]
skolemTerms vars v s
  | null d    = [Var (_SkolemConstTerm ++ show (skolemConstTermId v s))]
  | otherwise = map (Var) d
  where d = dependenceOf v s

dependenceOf :: ExistentialVariable -> QuantifierScope -> [UniversalVariable]
dependenceOf v s = [a | (a, b) <- (scopeOf v s), a /= _SkolemConstScope]

scopeOf :: ExistentialVariable -> QuantifierScope -> QuantifierScope
scopeOf v s = case (associationOf v s) of
  Just a -> case (elemIndex a s) of
    Just b -> take (b + 1) s
  _ -> []

associationOf :: ExistentialVariable -> QuantifierScope -> Maybe QuantifierAssociation
associationOf v s
  | length qa == 1  = Just (head qa)
  | otherwise       = Nothing
  where qa = [(u, e) | (u, e) <- s, elem v e]






-- 5. Elimination of Universal Quantifiers
type PrenexFormula = ([UniversalVariable], Matrix)
type Matrix = Formula

eliminateUniversalQuantifier :: Formula -> Matrix
eliminateUniversalQuantifier f = matrix f


matrix :: Formula -> Matrix
matrix f = snd (seg)
  where seg = segregateUQ [] f f

prenex :: [Variable] -> Formula -> Formula
prenex [] f = f
prenex (v:vars) f = ForAll v (prenex vars f)

segregateUQ :: [Variable] -> Formula -> Formula -> ([Variable], Formula)
segregateUQ vars formula f@(Predicate n t) = (vars, formula)
segregateUQ vars formula (Negation f) = segregateUQ vars  (unqalify formula) f
segregateUQ vars formula (Disj fs) = (vars ++ allUniversalVars fs, unqalify (Disj fs))
segregateUQ vars formula (Conj fs) = (vars ++ allUniversalVars fs, unqalify (Conj fs))
segregateUQ vars formula (ForAll v f) = segregateUQ (vars ++ [v]) (unqalify formula) f

unqalify :: Formula -> Formula
unqalify f@(Predicate n t) = f
unqalify (Negation f) = Negation f
unqalify (Disj fs) = Disj (map unqalify fs)
unqalify (Conj fs) = Conj (map unqalify fs)
unqalify (Exists v f) = f
unqalify (ForAll v f) = f

allUniversalVars :: [Formula] -> [UniversalVariable]
allUniversalVars [] = []
allUniversalVars (f:fs) = universalVars f ++ allUniversalVars fs

universalVars :: Formula -> [UniversalVariable]
universalVars (Predicate n t) = []
universalVars (Negation f) = universalVars f
universalVars (Conj fs) = allUniversalVars fs
universalVars (Disj fs) = allUniversalVars fs
universalVars (ForAll v f) = [v] ++ universalVars f






-- 6. Flattering conjunctions and disjunctions
flatter :: Formula -> Formula
flatter f@(Predicate n terms) = f
flatter (Negation f) = Negation (flatter f)
flatter (Conj fs) = Conj (flatterConjs fs)
flatter (Disj fs) = Disj (flatterDisjs fs)
flatter (ForAll v f) = ForAll v (flatter f)
flatter (Exists v f) = Exists v (flatter f)

flatterConjs [] = []
flatterConjs (f:fs) = flatterConj f ++ flatterConjs fs
flatterConj (Conj fs) = fs
flatterConj f = [flatter f]

flatterDisjs [] = []
flatterDisjs (f:fs) = flatterDisj f ++ flatterDisjs fs
flatterDisj (Disj fs) = fs
flatterDisj f = [flatter f]
