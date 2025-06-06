{-# LANGUAGE OverloadedStrings #-}

module Test.Certs where

import Crypto.PubKey.ECC.Generate (generate)
import Crypto.PubKey.ECC.Types (CurveName (SEC_p384r1), getCurveByName)
import Crypto.PubKey.ECC.ECDSA (PublicKey (public_curve), PrivateKey, toPublicKey, KeyPair, public_q)
import Data.ASN1.OID (OIDable (..), OID)
import Data.ByteString qualified as B
import Data.Hourglass (DateTime, Hours (..), dateAddPeriod, timeAdd)
import Data.Proxy (Proxy (..))
import Data.X509 qualified as X509
import Time.System (dateCurrent)

mkKeyPair :: IO (PublicKey, PrivateKey)
mkKeyPair = generate (getCurveByName SEC_p384r1)

-- toPubKeyEC :: PublicKey -> X509.PubKey
-- toPubKeyEC pubKey =
--   let curve = public_curve pubKey
--       point = public_q pubKey
--       oid :: OID = [1,3,132,0,34]
--   in X509.PubKeyEC $ X509.PubKeyEC_Named SEC_p384r1 _

-- mkRootCertificate :: KeyPair -> IO X509.Certificate
-- mkRootCertificate privKey = do
--   currentDate <- dateCurrent
--   let
--     pubKey = toPublicKey privKey
--     issuer =
--       X509.DistinguishedName
--         [(getObjectID X509.DnCommonName, "Trust Anchor")]
--     subject = issuer -- self signed
--     validity = (currentDate, timeAdd currentDate (Hours $ 24 * 365 * 10))
--   pure $
--     X509.Certificate
--       { X509.certVersion = 2
--       , X509.certSerial = 1
--       , X509.certSignatureAlg = X509.SignatureALG X509.HashSHA256 X509.PubKeyALG_Ed25519
--       , X509.certIssuerDN = issuer
--       , X509.certValidity = (currentDate, timeAdd currentDate (Hours $ 24 * 365 * 10))
--       , X509.certSubjectDN = subject
--       , X509.certPubKey = pubKey
--       , X509.certExtensions =
--           X509.Extensions
--             . Just
--             $ X509.extensionEncode True
--               <$> [ X509.ExtBasicConstraints True (Just 0)
--                   ]
--       }
