{-# LANGUAGE TemplateHaskell #-}

module TH where

import Data.Kind (Constraint, Type)
import Data.Maybe (catMaybes)
import Language.Haskell.TH

-- | Goal - Make this geneated function return Either String HandleableMsg
generateHandler :: Name -> Name -> [Name] -> Q [Dec]
generateHandler typeName outputName allowedTypes = do
  TyConI (DataD _ _ _ _ constructors _) <- reify typeName -- Maybe
  -- change to
  let isAllowedType t = t `elem` allowedTypes
  let mkClause :: Con -> Q (Maybe Clause)
      mkClause (NormalC conName [(_, innerType)]) = do
        case innerType of
          ConT t | isAllowedType t -> do
            x <- newName "x"
            let ptrn = conP conName [varP x]
            let body = normalB [|HandleableMsg $(varE x)|]
            Just <$> clause [ptrn] body []
          _ -> pure Nothing
      mkClause _ = pure Nothing
  clauses <- catMaybes <$> mapM mkClause constructors
  let funName = mkName "envelopeToHandleable"
  let typeSig = SigD funName (AppT (AppT ArrowT (ConT typeName)) (ConT outputName))
  let function = FunD funName clauses
  return [typeSig, function]

-- Our goal is to make a function that forms a sort of existential
-- type. Instead of having just one constructor, the existential type
-- might have many, for example:
--
-- data Foo
--   = forall a. (Show a) => Foo1 a
--   | forall a. (Show a) => Foo2 a
--   | forall a. (Show a) => Foo3 a
--
-- We also want to be able to specify the constraint

-- class CoreHandlable_ a where
--   coreHandle :: a -> IO ()

-- data CoreHandlable = forall a. (CoreHandlable_ a) => CoreHandlable a

-- generate ::
--   -- | The name of the resulting function
--   String ->
--   -- | Name of the Protobuf envelope type
--   Name ->
--   -- | Name of the existential output type
--   Name ->
--   -- | A sublist of the wrapped protobuf types that we want to allow
--   -- in the generated function.
--   [Name] ->
--   Q [Dec]
-- generate fnName pbTypeName outputTypeName [] = pure []
-- generate fnName pbTypeName outputTypeName allowedTypes =
--   let funName = mkName fnName
--    in do
--         functionImpl <- funImpl funName
--         pure [funSig funName, functionImpl]
--   where
--     funSig name =
--       SigD name (AppT (AppT ArrowT (ConT pbTypeName)) (ConT outputTypeName))

--     funImpl name =
--       FunD name <$> clauses

--     clauses = do
--       -- Get List of constructors for PB envelope.
--       TyConI (DataD _ _ _ _ constructors _) <- reify pbTypeName
--       pure $ concatMap mkClause constructors

--     mkClause :: Con -> [Clause]
--     mkClause (NormalC cName [(_, ConT innerType)]) =
--       if innerType `notElem` allowedTypes then [] else _
--     mkClause _ = []
