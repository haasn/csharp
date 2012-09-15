{-# LANGUAGE DeriveFunctor, FlexibleInstances #-}

data Free f a = Pure a | Free (f (Free f a))

-- Monad implementation
instance Functor f => Monad (Free f) where
  return = Pure
  (Pure a) >>= f = f a
  (Free f) >>= g = Free $ fmap (>>= g) f

-- liftF injection
liftF = Free . fmap Pure

-- Example: Toy language

data Toy next = Output String next | Bell next | Done
  deriving Functor

-- Smart constructors
output x = liftF $ Output x ()
bell     = liftF $ Bell ()
done     = liftF   Done

-- Normal and pure interpreters

interpret (Free (Output s n)) = putStrLn s       >> interpret n
interpret (Free (Bell n))     = putStrLn "Bell!" >> interpret n
interpret (Free Done)         = return ()
interpret (Pure _)            = error "Incorrect termination."

instance Show (Free Toy a) where
  show (Free (Output s n)) = "Output(" ++ s ++ ")\n" ++ show n
  show (Free (Bell n))     = "Bell()\n"              ++ show n
  show (Free Done)         = "Done()"
  show (Pure _)            = "<< CONT >>"

-- Example program

subroutine = do
  output "Foo"
  output "Bar"

program = do
  subroutine
  bell
  done

main = do
  -- Show program first
  print program

  -- Wait for keypress
  putStrLn "--- Press any key to run ---"
  getChar

  -- Interpret normally
  interpret program
