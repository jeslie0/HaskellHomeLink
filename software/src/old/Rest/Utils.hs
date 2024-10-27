{-# LANGUAGE DataKinds #-}
module Rest.Utils where

import Error(ErrorStack, ErrorT(..))
import TinyServant.Server (Server)
import TinyServant.API.UVerb (UVerb, WithStatus(..))
import TinyServant.API (StdMethod(GET))
import TinyServant.API.ContentTypes (JSON)
import TinyServant.Server.UVerb (respond)

eitherErrorStackToServer :: forall a. ErrorT a -> Server (UVerb GET '[JSON] '[WithStatus 200 a, WithStatus 500 ErrorStack])
eitherErrorStackToServer (ErrorT (Left stack)) = respond @(WithStatus 500 ErrorStack) $ WithStatus stack
eitherErrorStackToServer (ErrorT (Right b)) = respond @(WithStatus 200 a) $ WithStatus b
