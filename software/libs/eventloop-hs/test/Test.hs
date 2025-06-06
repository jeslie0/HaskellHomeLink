module Test where

import RxTx.Connection.Socket (aquireClientSocket, connectToHost)
import RxTx.ConnectionRegistry (mkConnectionRegistry)

main :: IO Int
main = do
  pure 1
