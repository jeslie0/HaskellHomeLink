module ErrorStack where

-- Define the ErrorStack data type
data ErrorStack a = Error [String]  -- Error case: a stack of errors
                  | Success a       -- Success case: the computed value
                  deriving (Show, Eq)

-- Functor instance for ErrorStack
instance Functor ErrorStack where
  fmap f (Success a) = Success (f a)
  fmap _ (Error errs) = Error errs

-- Applicative instance for ErrorStack (this allows error accumulation)
instance Applicative ErrorStack where
  pure = Success
  (Success f) <*> (Success a) = Success (f a)
  (Error errs1) <*> (Error errs2) = Error (errs1 ++ errs2)  -- Accumulate errors from both
  (Error errs) <*> _ = Error errs   -- Error on left propagates
  _ <*> (Error errs) = Error errs   -- Error on right propagates

-- Helper to add an error to the stack
addError :: String -> ErrorStack a -> ErrorStack a
addError err (Error errs) = Error (err:errs)
addError err (Success _)  = Error [err]

-- Example computations with potential errors
computation1 :: Int -> ErrorStack Int
computation1 x
  | x < 0     = Error ["computation1: Input is less than zero"]
  | otherwise = Success (x + 10)

computation2 :: Int -> ErrorStack Int
computation2 x
  | x == 0    = Error ["computation2: Division by zero"]
  | otherwise = Success (100 `div` x)

computation3 :: Int -> ErrorStack Int
computation3 x
  | x < 5     = Error ["computation3: Input is less than 5"]
  | otherwise = Success (x * 2)

-- A sequence of computations that can fail, adding an error if computation1 fails
process :: Int -> ErrorStack Int
process x = addError "computation2 ran after computation1 failed" <$> computation1 x <*> computation2 x <*> computation3 x

-- Usage example
main :: IO ()
main = do
  -- Success case
  print $ process 5   -- Should succeed with no errors

  -- Error in the first computation, but computation2 adds a new error
  print $ process (-5) -- Should fail in computation1 and add error from computation2

  -- Error in computation2 after computation1 succeeds
  print $ process 0    -- Should fail in computation2 with division by zero
