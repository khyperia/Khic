module BackendLLVM.Compile(compile) where

import LLVM.General.AST as Ast
import LLVM.General.Target
import LLVM.General.Module as Mod
import LLVM.General.Context
import Control.Monad.Error

extractError :: (Monad m) => ErrorT String m a -> m a
extractError = liftM switch . runErrorT
  where switch (Left x) = error x
        switch (Right x) = x

compile :: Ast.Module -> IO ()
compile m = withContext $ \context ->
            extractError $ withDefaultTargetMachine $ \machine ->
            extractError $ withModuleFromAST context m $ \llvmMod -> do
            mstr <- moduleString llvmMod
            putStrLn mstr
            extractError $ writeObjectToFile machine "a.obj" llvmMod
