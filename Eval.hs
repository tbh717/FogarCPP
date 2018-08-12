module Eval where

import Data.Maybe
import Data.Fixed -- for floating point modulo

{----------
-- TYPES --
----------}

type Var = String
type Loc = Int

type Env = [(Var, Value)]
type Store = [(Loc, Value)]
data Context = Context { env :: Env, store :: Store }

-- Value type --
data Value = 
    IntV Int | RealV Float | 
    BoolV Bool | 
    StringV String |
    PairV Value Value |
    FuncV Env Var Expr |
    RefV Loc |
    UnitV

instance Eq Value where
    x == y = case (x,y) of
        (IntV a, IntV b) -> a == b
        (IntV a, RealV b) -> (fromIntegral a) == b
        (RealV a, IntV b) -> a == (fromIntegral b)
        (RealV a, RealV b) -> a == b
        (StringV a, StringV b) -> a == b
        (PairV a b, PairV c d) -> (a == c) && (b == d)
        (BoolV a, BoolV b) -> a == b
        (RefV a, RefV b) -> a == b
        (UnitV, UnitV) -> True
        _ -> False

-- Expr subtypes --
data Const = LifeC | PiC | TauC | FogarteC deriving (Eq)
type BinFunc = (Value -> Value -> (Maybe Value))
type UnFunc = (Value -> (Maybe Value))
type PairFunc = (Value -> (Maybe Value))
type CompFunc = (Float -> Float -> Bool)

-- Expr types --
data Statement = 
    Ex Expr | 
    GlobalV Var Expr | 
    RecurV Var Expr

data Expr = 
    Expr Expr | 
    -- Value types
    Int Int | Real Float | Bool Bool | String String | Pair Expr Expr |
    Func Var Expr | Unit | Const Const | Var Var | 
    -- Function application
    App Expr Expr |
    BinOp BinFunc Expr Expr |
    UnOp UnFunc Expr | PairOp PairFunc Expr |
    CompOp CompFunc Expr Expr | EqOp Expr Expr | NEqOp Expr Expr |
    BothOp Expr Expr | OneOp Expr Expr | NotOp Expr |
    -- If/then/else 
    Cond Expr Expr Expr |
    -- Local variable declaration
    LocalV Var Expr Expr |
    -- References
    Ref Expr | Deref Expr | Mut Expr Expr |
    -- Sequences
    Seq Expr Expr |
    -- Do-while
    Loop Expr Expr

{-------------
-- CONTEXTS --
--------------}

updateMap :: Eq a => [(a,b)] -> a -> b -> [(a,b)]
updateMap [] var val = [(var,val)]
updateMap (asc@(varCurr, valCurr):rest) var val = 
    if(varCurr == var) 
        then (var, val):rest
        else asc:(updateMap rest var val)

newLoc :: Store -> Loc
newLoc [] = 1
newLoc store = let max = maximum (map fst store)
    in max + 1

{-----------------------
-- HIGHER-LEVEL FUNCS --
------------------------}

appMaybe :: (a -> Maybe b) -> Maybe a -> Maybe b
appMaybe f a = do 
    av <- a
    f av

maybeFst :: Maybe (a,b) -> Maybe a
maybeFst mpair = do
    pair <- mpair
    Just $ fst pair

maybeSnd :: Maybe (a,b) -> Maybe b
maybeSnd mpair = do
    pair <- mpair
    Just $ snd pair

threadStore1 :: (Value -> Maybe Value) -> Context -> Expr -> Maybe (Value, Store)
threadStore1 f c@(Context {store=cstore, env=cenv}) exp = do
    (v,cstore') <- evalE c exp
    v' <- f v
    Just $ (v',cstore')

threadStore2 :: (Value -> Value -> Maybe Value) -> Context -> Expr -> Expr -> Maybe (Value, Store)
threadStore2 f c@(Context {store=cstore, env=cenv}) exp1 exp2 = do
    (v1,cstore') <- (evalE c exp1)
    (v2,cstore'') <- (evalE c {store = cstore'} exp2)
    v' <- f v1 v2
    Just $ (v',cstore'')

{---------
-- EVAL --
----------}

evalS :: Context -> Statement -> (Maybe (Value, Context))
-- evalS _ _ = Nothing
evalS c@(Context {store=cstore, env=cenv}) s = case s of
    (Ex exp) -> do
        (val,cstore') <- (evalE c exp)
        Just $ (val, c { store=cstore' })
    (GlobalV var exp) -> do
        (val, cstore') <- (evalE c exp)
        let env' = updateMap cenv var val
        Just $ (UnitV, c { env = env', store = cstore' })
    (RecurV var exp) ->
        let rf = (evalE c {env = (updateMap env' var (fromMaybe UnitV (maybeFst rf)))} exp)
            env' = updateMap cenv var (fromMaybe UnitV (maybeFst rf))
            cstore' = cstore ++ (fromMaybe [] (maybeSnd rf))
        in Just $ (UnitV, c {env = env', store = cstore'})

-- Expressions
evalE :: Context -> Expr -> Maybe (Value, Store)
evalE c@(Context {store=cstore, env=cenv}) exp = case exp of
    -- Expressions
    Expr exp' -> evalE c exp'
    -- Types
    Int v -> Just $ (IntV v, cstore)
    Real v -> Just $ (RealV v, cstore)
    Bool v -> Just $ (BoolV v, cstore)
    String v -> Just $ (StringV v, cstore)
    Pair e1 e2 -> do
        (v1,cstore') <- (evalE c e1)
        (v2,cstore'') <- (evalE c {store = cstore'} e2)
        Just $ (PairV v1 v2, cstore'')
    Func var exp' -> Just $ (FuncV cenv var exp', cstore)
    Unit -> Just $ (UnitV, cstore)
    Const v -> do
        const <- constV v
        Just $ (const, cstore)
    -- Variables
    Var v -> do
        val <- lookup v cenv
        Just $ (val, cstore)
    LocalV var x y -> do
        (valx, cstore') <- evalE c x
        let env' = updateMap cenv var valx
        evalE (c { env = env', store=cstore' }) y 
    -- User-defined Functions
    App f exp' -> do
        (fv,cstore') <- (evalE c f)
        case fv of
            (FuncV env' var exp'') -> do
                (val,cstore'') <- evalE c exp'
                let env'' = updateMap env' var val
                evalE (c {env=env'', store=cstore''}) exp''
            _ -> Nothing
    -- Built-in Functions
    UnOp f exp1 -> threadStore1 f c exp1
    BinOp f exp1 exp2 -> threadStore2 f c exp1 exp2
    -- Conditionals
    CompOp f exp1 exp2 -> do
        (val1,cstore') <- evalE c exp1
        (val2,cstore'') <- evalE (c {store=cstore'}) exp2
        val <- comp f val1 val2
        Just $ (val, cstore'')
    EqOp exp1 exp2 -> threadStore2 equality c exp1 exp2
    NEqOp exp1 exp2 -> threadStore2 nequality c exp1 exp2
    -- Conditionals
    BothOp exp1 exp2 -> case (evalE c exp1) of
        Just (BoolV False, _) -> Just $ (BoolV False, cstore)
        Just (BoolV True, _) -> case (evalE c exp2) of
            Just (BoolV False, _) -> Just $ (BoolV False, cstore)
            Just (BoolV True, _) -> Just $ (BoolV True, cstore)
            _ -> Nothing
        _ -> Nothing
    OneOp exp1 exp2 -> case (evalE c exp1) of
        Just (BoolV True, cstore') -> Just (BoolV True, cstore)
        Just (BoolV False, cstore') -> case (evalE c exp2) of
            Just (BoolV True, cstore'') -> Just $ (BoolV True, cstore)
            Just (BoolV False, cstore'') -> Just $ (BoolV False, cstore)
            _ -> Nothing
        _ -> Nothing
    NotOp exp' -> case (evalE c exp') of
        Just (BoolV True, cstore') -> Just (BoolV False, cstore)
        Just (BoolV False, cstore') -> Just (BoolV True, cstore)
        _ -> Nothing
    Cond ifExp thenExp elseExp -> do
        (ifVal, cstore') <- (evalE c ifExp)
        case ifVal of
            (BoolV True) -> (evalE (c {store = cstore'}) thenExp)
            (BoolV False) -> (evalE (c {store = cstore'}) elseExp)
            _ -> Nothing
    -- Pairs
    PairOp f exp1 -> threadStore1 f c exp1
    -- References
    Ref exp' -> do
        (val, _) <- evalE c exp'
        let loc = newLoc cstore
            cstore' = updateMap cstore loc val
        Just $ (RefV loc, cstore')
    Deref exp' -> do
        (ref, cstore') <- evalE c exp'
        case ref of
            (RefV loc) -> do
                val <- lookup loc cstore'
                Just $ (val, cstore') 
            _ -> Nothing
    Mut refexp newexp -> do
        (ref, cstore') <- evalE c refexp
        case ref of
            (RefV loc) -> do
                (val, cstore'') <- evalE (c {store = cstore'}) newexp
                let final_cstore = updateMap cstore'' loc val
                Just $ (UnitV, final_cstore)
            _ -> Nothing
    -- Sequences
    Seq exp1 exp2 -> do
        (_, cstore') <- evalE c exp1
        evalE (c {store = cstore'}) exp2
    -- While loop
    Loop condition action -> do
        (continue, cstore') <- evalE c condition
        case continue of 
            (BoolV False) -> Just $ (UnitV, cstore')
            (BoolV True) -> do
                (val, cstore'') <- evalE (c {store = cstore'}) action
                evalE (c {store = cstore''}) (Loop condition action)
            _ -> Nothing

{----------
-- FUNCS --
-----------}

arithOp :: (Int -> Int -> Int) -> (Float -> Float -> Float) -> Value -> Value -> (Maybe Value)
arithOp intfunc floatfunc a b = case a of (IntV x) -> case b of (IntV y) -> Just $ IntV (intfunc x y)
                                                                (RealV y) -> Just $ RealV (floatfunc (fromIntegral x) y)
                                                                otherwise -> Nothing
                                          (RealV x) -> case b of (IntV y) -> Just $ RealV (floatfunc x (fromIntegral y))
                                                                 (RealV y) -> Just $ RealV (floatfunc x y)
                                                                 otherwise -> Nothing
                                          otherwise -> Nothing

unOpMaybe :: (a -> Maybe b) -> Maybe a -> Maybe b
unOpMaybe f a = do av <- a
                   f av

binOpMaybe :: (a -> b -> Maybe c) -> Maybe a -> Maybe b -> Maybe c
binOpMaybe f a b = do av <- a
                      bv <- b
                      f av bv

{- Constants -}

constV :: Const -> (Maybe Value)
constV c = case c of
  LifeC -> Just $ IntV (42)
  PiC -> Just $ RealV (pi)
  TauC -> Just $ RealV (2.0*pi)
  FogarteC -> Just $ RealV (exp 1)

{- Binary ops -}

plusV :: Value -> Value -> (Maybe Value)
plusV x y = arithOp (+) (+) x y

minusV :: Value -> Value -> (Maybe Value)
minusV x y = arithOp (-) (-) x y

multV :: Value -> Value -> (Maybe Value)
multV x y = arithOp (*) (*) x y

divV :: Value -> Value -> (Maybe Value)
divV (IntV a) (IntV b) = Just $ RealV ((fromIntegral a) / (fromIntegral b))
divV (RealV a) (IntV b) = Just $ RealV (a / (fromIntegral b))
divV (IntV a) (RealV b) = Just $ RealV ((fromIntegral a) / b)
divV (RealV a) (RealV b) = Just $ RealV (a / b)
divV _ _ = Nothing

divIntV :: Value -> Value -> (Maybe Value)
divIntV (IntV a) (IntV b) = Just $ IntV (a `div` b)
divIntV (RealV a) (IntV b) = Just $ IntV ((floor a) `div` b)
divIntV (IntV a) (RealV b) = Just $ IntV (a `div` (floor b))
divIntV (RealV a) (RealV b) = Just $ IntV ((floor a) `div` (floor b))
divIntV _ _ = Nothing

modV :: Value -> Value -> (Maybe Value)
modV x y = arithOp (mod) (mod') x y

expV :: Value -> Value -> (Maybe Value)
expV (IntV x) (IntV y) = Just $ IntV (x^y)
expV (RealV x) (IntV y) = Just $ RealV (x^y)
expV _ _ = Nothing

{- Unary ops -}

negateV :: Value -> (Maybe Value)
negateV (IntV x) = Just $ IntV (-x)
negateV (RealV x) = Just $ RealV (-x)
negateV _ = Nothing

roundV :: Value -> (Maybe Value)
roundV (IntV x) = Just $ IntV (x)
roundV (RealV x) = Just $ IntV (round x)
roundV _ = Nothing

{- Conditionals -}

ifV :: (Maybe Value) -> (Maybe Value) -> (Maybe Value) -> (Maybe Value)
ifV x y z = do
  ifVal <- x
  thenVal <- y
  elseVal <- z
  case ifVal of (BoolV True) -> (Just thenVal)
                (BoolV False) -> (Just elseVal)
                _ -> Nothing

{- Comparison function -}

comp :: CompFunc -> Value -> Value -> (Maybe Value)
comp f x y = case x of 
    (RealV xv) -> case y of 
      (RealV yv) -> Just $ BoolV (f xv yv)
      (IntV yv) -> Just $ BoolV (f xv (fromIntegral yv))
      _ -> Nothing
    (IntV xv) -> case y of 
      (RealV yv) -> Just $ BoolV (f (fromIntegral xv) yv)
      (IntV yv) -> Just $ BoolV (f (fromIntegral xv) (fromIntegral yv))
      _ -> Nothing
    _ -> Nothing

equality :: Value -> Value -> (Maybe Value)
equality x y = if x == y
    then Just $ BoolV True
    else Just $ BoolV False

nequality :: Value -> Value -> (Maybe Value)
nequality x y = if x /= y
    then Just $ BoolV True
    else Just $ BoolV False

{- Pair ops -}

fstP :: Value -> (Maybe Value)
fstP (PairV x y) = Just x
fstP _ = Nothing

sndP :: Value -> (Maybe Value)
sndP (PairV x y) = Just y
sndP _ = Nothing

{-------------
-- PRINTING --
--------------}

printVal :: Context -> Value -> (Maybe String)
printVal c (RefV loc) = do
    val <- lookup loc (store c)
    Just $ (show val)
printVal _ val = Just $ (show val)

enclose :: String -> String
enclose s = "{ " ++ s ++ " }"

instance Show Statement where
    show (Ex e) = "{" ++ (show e) ++ "}"
    show (GlobalV v e) = "SIR " ++ v ++ " THE " ++ (show e)
    show (RecurV v e) = "DAME " ++ v ++ " THE " ++ (show e)

instance Show Expr where
    show (Expr e) = show e
    show (Int v) = show v
    show (Real v) = show v
    show (Bool v) = if(v) then "Knight" else "Knave"
    show (String v) = v
    show (Pair e1 e2) = "(" ++ (show e1) ++ ", " ++ (show e2) ++ ")"
    show (Func var e) = "FUNCTION (" ++ var ++ "): " ++ (show e)
    show (Unit) = "stump"
    show (Const c) = (show c)
    show (Var v) = "Var " ++ v
    show (App f e) = "[" ++ (show f) ++ " (" ++ (show e) ++ ")" ++ "]"
    show (BinOp f e1 e2) = (show e1) ++ " BinOp " ++ (show e2)
    show (UnOp f e) = "UnOp " ++ (show e)
    show (PairOp f e) = "PairOp " ++ (show e)
    show (CompOp f e1 e2) = (show e1) ++ " CompOp " ++ (show e2)
    show (EqOp e1 e2) = (show e1) ++ " EqOp " ++ (show e2)
    show (BothOp e1 e2) = (show e1) ++ " BothOp " ++ (show e2)
    show (OneOp e1 e2) = (show e1) ++ " OneOp " ++ (show e2)
    show (NotOp e) = "NotOp " ++ (show e)
    show (Cond e1 e2 e3) = "IF " ++ (show e1) ++ " THEN " ++ (show e2) ++ " ELSE " ++ (show e3)
    show (LocalV v e1 e2) = "SIR " ++ v ++ " THE " ++ (show e1) ++ " LORD OF " ++ (show e2)
    show (Ref e) = "Ref " ++ (show e)
    show (Deref e) = "Deref " ++ (show e)
    show (Mut e1 e2) = (show e1) ++ " steals " ++ (show e2)
    show (Seq e1 e2) = (show e1) ++ " | " ++ (show e2)
    show (Loop e1 e2) = "calm " ++ (show e1) ++ " maeb " ++ (show e2)   

instance Show Value where
    show (IntV x) = show x
    show (RealV x) = show x
    show (BoolV x) = case x of 
        True -> "knight"
        False -> "knave"
    show (StringV x) = "\"" ++ x ++ "\""
    show (PairV x y) = "(" ++ show x ++ ", " ++ show y ++ ")"
    show (FuncV env vname expr) = "FUNCTION (var: " ++ vname ++ ", env: " ++ show env ++ "): " ++ (show expr)
    show (RefV loc) = "REF: " ++ show loc
    show (UnitV) = "stump"

instance Show Const where
    show (LifeC) = "life"
    show (PiC) = "pi"
    show (TauC) = "tau"
    show (FogarteC) = "fogarte"