module Kitten.Compile
  ( compile
  , typecheck
  ) where

import Control.Monad
import Data.Vector (Vector)

import Kitten.Def
import Kitten.Error
import Kitten.Fragment
import Kitten.Resolve
import Kitten.Scope
import Kitten.Term
import Kitten.Token
import Kitten.Type
import Kitten.Type.Infer

import qualified Kitten.Resolve as Resolve

compile
  :: [Resolve.Value]
  -> Vector (Def Resolved)
  -> String
  -> String
  -> Either CompileError (Fragment Resolved)
compile stack prelude name source = do
  tokenized <- failIfError $ tokenize name source
  parsed <- failIfError $ parse name tokenized
  resolved <- resolveFragment prelude parsed
  scoped <- scopeFragment resolved
  void $ typeFragment prelude stack resolved
  return scoped

typecheck
  :: [Resolve.Value]
  -> Vector (Def Resolved)
  -> String
  -> String
  -> Either CompileError Type
typecheck stack prelude name
  = failIfError . tokenize name
  >=> failIfError . parse name
  >=> resolveFragment prelude
  >=> liftM (manifestType . fragmentTerm) . typeFragment prelude stack
