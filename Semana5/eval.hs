import Data.Char
import Data.Maybe
import Data.Either
import qualified Data.Map      as DM
import Data.Sequence as DS
import Data.Foldable as DF
import Data.List
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Error
import Control.Monad.Reader
import Control.Applicative ((<*>))
import Text.ParserCombinators.Parsec hiding (State)

data Exp = Const Int
         | Var String
         | Add Exp Exp
         | Sub Exp Exp
         | Mul Exp Exp
         | Div Exp Exp
         | Empty
         deriving (Eq,Show)

env = DM.fromList [ ("foo",42), ("bar",69), ("baz",27), ("qux",0) ]

eval (Const i) = i
eval (Var n)   = fromJust (DM.lookup n env)
eval (Add l r) = (eval l) + (eval r)
eval (Sub l r) = (eval l) - (eval r)
eval (Mul l r) = (eval l) * (eval r)
eval (Div l r) = (eval l) `div` (eval r)

ex1 = (Const 21)
ex2 = (Var "foo")
ex3 = (Mul (Const 2) ex1)
ex4 = (Add (Mul ex2 (Const 5)) (ex3))
ex5 = (Add (Div (Const 42) (Const 2)) (Mul (Const 3) (Const 7)))

data EvalState = EvalState { adds, subs, muls, divs, vars :: Int }
               deriving (Show)

initialState = EvalState { adds = 0, subs = 0, muls = 0, divs = 0, vars = 0 }

evalST :: Exp -> State EvalState Int
evalST (Const i) = return i
evalST (Var n)   = do
  s <- get
  let v = maybe (error $ "Var "++ n ++ " not found") id (DM.lookup n env)
  put $ s { vars = vars s + 1 }
  return v
evalST (Add l r) = do
  vl <- evalST l
  vr <- evalST r
  s <- get
  put $ s { adds = adds s + 1 }
  return $ vl + vr
evalST (Mul l r) = do
  vl <- evalST l
  vr <- evalST r
  s <- get
  put $ s { muls = muls s + 1 }
  return $ vl * vr
evalST (Sub l r) = do
  vl <- evalST l
  vr <- evalST r
  s <- get
  put $ s { subs = subs s + 1 }
  return $ vl - vr
evalST (Div l r) = do
  vl <- evalST l
  vr <- evalST r
  s <- get
  put $ s { divs = divs s + 1 }
  return $ vl `div` vr

evalwithstats :: Exp -> IO ()
evalwithstats e = putStr $ unlines $ [ "Result: " ++ show r ] ++ [ show s ]
  where (r,s) = runState (evalST e) initialState

evalWR :: Exp -> Writer (Seq String) Int
evalWR t@(Const i) = do
  tell $ logExp t i
  return i
evalWR t@(Var n)   = do
  let v = maybe (error $ "Var "++ n ++ " not found") id (DM.lookup n env)
  tell $ logExp t v
  return v
evalWR t@(Add l r) = do
  vl <- evalWR l
  vr <- evalWR r
  let v = vl + vr
  tell $ logExp t v
  return $ v
evalWR t@(Sub l r) = do
  vl <- evalWR l
  vr <- evalWR r
  let v = vl - vr
  tell $ logExp t v
  return $ v
evalWR t@(Mul l r) = do
  vl <- evalWR l
  vr <- evalWR r
  let v = vl * vr
  tell $ logExp t v
  return $ v
evalWR t@(Div l r) = do
  vl <- evalWR l
  vr <- evalWR r
  let v = vl `div` vr
  tell $ logExp t v
  return $ v

logExp :: Exp -> Int -> Seq String
logExp e v = singleton $ "Exp: " ++ show e ++ " -> Val: " ++ show v

evalwithlog :: Exp -> IO ()
evalwithlog e = putStr $ unlines $ [ "Result: " ++ show r ] ++ DF.toList l
  where (r,l) = runWriter (evalWR e)

data ExpError = DivisionPorCero
              | NumeroDeMalaSuerte
              | VariableNoExiste String
              deriving (Show)

instance Error ExpError

evalEX :: Exp -> Either ExpError Int
evalEX (Const i) = checkForThirteen i
evalEX (Var n)   = maybe (throwError $ VariableNoExiste n) 
                         checkForThirteen 
                         (DM.lookup n env)
evalEX (Add l r) = checkMath (+) l r
evalEX (Sub l r) = checkMath (-) l r
evalEX (Mul l r) = checkMath (*) l r
evalEX (Div l r) = do
  vl <- evalEX l
  vr <- evalEX r
  if vr == 0 then throwError DivisionPorCero
             else checkForThirteen $ vl `div` vr

checkMath op l r = liftM2 (op) (evalEX l) (evalEX r) >>= checkForThirteen

checkForThirteen 13 = throwError NumeroDeMalaSuerte
checkForThirteen i  = return i

evalwithcare :: Exp -> IO ()
evalwithcare e = either bad good (evalEX e)
  where good  = msg "Result: "
        bad   = msg "Rayos: "
        msg s = putStrLn . (++)s . show

evalRD :: Exp -> Reader (DM.Map String Int) Int
evalRD (Const i) = return i
evalRD (Var n)   = do
  asks (\e -> maybe (error $ "Var "++ n ++ " not found") id (DM.lookup n e))
evalRD (Add l r) = liftM2 (+) (evalRD l) (evalRD r)
evalRD (Sub l r) = liftM2 (-) (evalRD l) (evalRD r)
evalRD (Mul l r) = liftM2 (*) (evalRD l) (evalRD r)
evalRD (Div l r) = liftM2 div (evalRD l) (evalRD r)

evalwithenv :: DM.Map String Int -> Exp -> IO ()
evalwithenv a e = putStrLn $ "Result: " ++ (show $ runReader (evalRD e) a)
