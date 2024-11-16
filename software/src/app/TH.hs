{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module TH (
    makeInstance,
    makeToEnvelopeInstances,
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
        _ -> fail $ "Input name must be a class name." <> show info

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
    -- ^ Envelope type name
    -> Name
    -- ^ Payload constructor name
    -> Name
    -- ^ Payload type name
    -> Q [Dec]
makeInstance className typeName payloadConstructorName payloadType = do
    decs <- mkDecs
    let inst =
            InstanceD
                Nothing
                []
                (AppT (ConT className) (ConT typeName))
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
            envelopeName <- newName "envelope"
            loopName <- newName "loop"
            matches <- mkMatches methodName loopName
            let caseExpression =
                    UInfixE (VarE envelopeName) (VarE $ mkName "^.") (VarE payloadConstructorName)
            let bodyExpression =
                    CaseE caseExpression matches
            pure $ Clause [VarP loopName, VarP envelopeName] (NormalB bodyExpression) []

        mkMatches methodName loopName = do
            constructorNames <- getConstructorNames payloadType
            constructors <- forM constructorNames $ \constructorName -> do
                varName <- newName "variable"
                pure $
                    Match
                        (ConP 'Just [] [ConP constructorName [] [VarP varName]])
                        (NormalB $ AppE (AppE (VarE methodName) (VarE loopName)) (VarE varName))
                        []
            let failCase =
                    Match
                        WildP
                        ( NormalB $
                            AppE (VarE $ mkName "fail") (LitE $ StringL "Pattern match failure on envelope")
                        )
                        []
            pure $ constructors <> [failCase]

{- | This lets us generate instances to wrap protobuf messages into
their respective envelopes. A user of this must define a class such
as:
class ToEnvelope msg where
    toEnvelope :: msg -> Radio.Envelope

The following use will then create the required instances for each
part of the payload:
-}

{- $(makeToEnvelopeInstances ''ToEnvelope ''Radio.Envelope ''Radio.Envelope'Payload 'Radio.maybe'payload)

instance ToEnvelope StartRadio where
  toEnvelope msg_aa9hv
    = (maybe'payload ?~ Envelope'M1 msg_aa9hv) defMessage
instance ToEnvelope StopRadio where
  toEnvelope msg_aa9hw
    = (maybe'payload ?~ Envelope'M2 msg_aa9hw) defMessage
-}

makeToEnvelopeInstances ::
    Name
    -- ^ ToEnvelope class name
    -> Name
    -- ^ Envelope type name
    -> Name
    -- ^ Payload type name
    -> Name
    -- ^ Envelop Payload lens setter
    -> Q [Dec]
makeToEnvelopeInstances className _typeName payloadTypeName payloadConslensSetter = do
    classMethodNames <- getClassMethodNames className
    info <- reify payloadTypeName
    case info of
        TyConI (DataD _ _ _ _ constructors _) -> do
            forM constructors $ \con -> mkInstanceDec classMethodNames con
        _ -> fail "Couldn't match on data type"
  where
    mkInstanceDec classMethodNames con@(NormalC conName _) = do
        conInputType <- getNameOfConInputType con
        decs <- mkDecs conName classMethodNames
        pure $
            InstanceD
                Nothing
                []
                (AppT (ConT className) (ConT conInputType))
                decs
    mkInstanceDec _ _ = fail "Couldn't match constructor"

    mkDecs constructorName classMethodNames = do
        forM classMethodNames $ \method -> do
            msgName <- newName "msg"
            let body =
                    AppE
                        ( UInfixE
                            (VarE payloadConslensSetter)
                            (VarE $ mkName "?~")
                            (AppE (ConE constructorName) (VarE msgName))
                        )
                        (VarE $ mkName "defMessage")
            let methodClause = Clause [VarP msgName] (NormalB body) []
            pure $ FunD method [methodClause]

--
getNameOfConInputType :: Con -> Q Name
getNameOfConInputType (NormalC _ [(_, ConT typeName)]) = pure typeName
getNameOfConInputType _ = fail "Couldn't match conInputType"
