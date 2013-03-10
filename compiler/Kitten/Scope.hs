{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Kitten.Scope
  ( scopeFragment
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Writer

import qualified Data.Vector as Vector

import Kitten.Def
import Kitten.Error
import Kitten.Fragment
import Kitten.Name
import Kitten.Resolve

import qualified Kitten.Builtin as Builtin

scopeFragment
  :: Fragment Resolved
  -> Either CompileError (Fragment Resolved)
scopeFragment Fragment{..} = Fragment
  <$> Vector.mapM scopeDef fragmentDefs
  <*> evalScoping (scopeTerm 0 fragmentTerm)

scopeDef
  :: Def Resolved
  -> Either CompileError (Def Resolved)
scopeDef def@Def{..} = do
  scoped <- evalScoping $ scopeTerm 0 defTerm
  return $ def { defTerm = scoped }

type Depth = Int

newtype Scoping a = Scoping
  { unwrapScoping :: WriterT [Int] (Either CompileError) a }
  deriving (Applicative, Functor, Monad)

evalScoping :: Scoping a -> Either CompileError a
evalScoping = liftM fst . runWriterT . unwrapScoping

runScoping :: Scoping a -> Either CompileError (a, [Int])
runScoping = runWriterT . unwrapScoping

scopeTerm
  :: Depth
  -> Resolved
  -> Scoping Resolved

scopeTerm _depth resolved@(Builtin _) = return resolved

scopeTerm _depth (Compose terms)
  = Compose <$> Vector.mapM (scopeTerm 0) terms

scopeTerm depth (Scoped term)
  = scopeTerm (succ depth) term

scopeTerm _depth (Value (Fun term)) = do
  ns <- fun term
  let
    n = length ns
    locals = makeLocals ns
    body = makeScopes n term
    composes = makeComposes n
  return . Compose $ Vector.fromList [locals, body, composes]

scopeTerm _depth resolved@(Value _) = return resolved

scopeTerm depth resolved@(Local (Name index))
  = if index >= depth  -- local refers outside fun
    then do
      n <- fresh  -- replace local with fresh inside
      return $ Local n
    else return resolved

fresh :: (Monad m) => m a
fresh = return undefined

fun :: Resolved -> Scoping [Int]
fun _ = return []

makeLocals :: [Int] -> Resolved
makeLocals ns = Compose . Vector.map (Local . Name) $ Vector.fromList ns

makeScopes :: Int -> Resolved -> Resolved
makeScopes _ term = term

makeComposes :: Int -> Resolved
makeComposes n = Compose . Vector.fromList . replicate n
  $ Builtin Builtin.Compose
