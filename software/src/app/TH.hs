{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module TH (
    makeInstance,
) where

import Control.Monad (forM)
import Language.Haskell.TH

getClassMethodNames :: Name -> Q [Name]
getClassMethodNames className = do
    info <- reify className
    case info of
        ClassI (ClassD _ _ _ _ methods) _ ->
            forM methods $ \case
                SigD methodName _ -> pure methodName
                _ -> fail "This should't happen."
        _ -> fail "Input name must be a class name."

getConstructorNames :: Name -> Q [Name]
getConstructorNames typeName = do
    info <- reify typeName
    case info of
        TyConI (DataD _ _ _ _ constructors _) ->
            forM constructors $ \case
                (NormalC name _) -> pure name
                _ -> fail "Data type must have normal constructors"
        _ -> fail "Input name must be a data type"

{- | The purpose of this is to derive an instance declaration for
envelope types. It should only compile if there are instance
declarations for each of the types in the payload.

An example useage should generate an instance of the form:


\$(makeInstance ''HomeHandler ''Env ''Radio.Envelope 'Radio.maybe'payload ''Radio.Envelope'Payload)

instance HomeHandler Env Radio.Envelope where
  homeHandler envelope =
    case envelope ^. Radio.maybe'payload of
      Just (Radio.Envelope'M1 x) -> homeHandler x
      Just (Radio.Envelope'M2 x) -> homeHandler x
      _ -> fail "Pattern match failure on envelope."
-}
makeInstance ::
    Name
    -- ^ Class name
    -> Name
    -- ^ Env name
    -> Name
    -- ^ Envelope type name
    -> Name
    -- ^ Payload constructor name
    -> Name
    -- ^ Payload type name
    -> Q [Dec]
makeInstance className envName typeName payloadConstructorName payloadType = do
    decs <- mkDecs
    let inst =
            InstanceD
                Nothing
                []
                (AppT (AppT (ConT className) (ConT envName)) (ConT typeName))
                decs
    pure [inst]
  where
    mkDecs :: Q [Dec]
    mkDecs = do
        classMethodNames <- getClassMethodNames className
        forM classMethodNames $ \methodName -> do
            instanceClause <- mkClause methodName
            pure $ FunD methodName [instanceClause]
      where
        mkClause methodName = do
            envelopeName <- newName "variable"
            matches <- mkMatches methodName
            let caseExpression =
                    UInfixE (VarE envelopeName) (VarE $ mkName "^.") (VarE payloadConstructorName)
            let bodyExpression =
                    CaseE caseExpression matches
            pure $ Clause [VarP envelopeName] (NormalB bodyExpression) []

        mkMatches methodName = do
            constructorNames <- getConstructorNames payloadType
            constructors <- forM constructorNames $ \constructorName -> do
                varName <- newName "variable"
                pure $
                    Match
                        (ConP 'Just [] [ConP constructorName [] [VarP varName]])
                        (NormalB $ AppE (VarE methodName) (VarE varName))
                        []
            let failCase =
                    Match
                        WildP
                        ( NormalB $
                            AppE (VarE $ mkName "fail") (LitE $ StringL "Pattern match failure on envelope")
                        )
                        []
            pure $ constructors <> [failCase]
