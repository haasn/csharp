{-# LANGUAGE DeriveFunctor #-}

import Prelude hiding (succ)
import Control.Monad (join)
import Control.Monad.Instances

-- Functor fixed point

newtype Mu f = In { out :: f (Mu f) }

cata phi = phi . fmap (cata phi) . out
ana  psi = In  . fmap (ana  psi) . psi

hylo phi psi = cata phi . ana psi

-- Example functor: List

data Cons a f = Empty | Cons a f deriving Functor

-- Elimination primitive

lelim z _  Empty     = z
lelim _ f (Cons a b) = f a b

-- Mu-lifted cons/empty

empty  = In Empty
cons a = In . Cons a

-- Instead of Nat the built-in Maybe type is used

zero = In Nothing
succ = In . Just

add a = cata (maybe a succ)
mul a = cata (maybe zero (add a))

-- Demo program

fac = hylo (lelim (succ zero) mul) (maybe Empty (join (Cons . succ)) . out)

main = putStrLn $ "fac " ++ toInt mynum ++ " = " ++ toInt (fac mynum)
  where
    mynum = succ . succ . succ . succ . succ $ zero
    toInt = show . cata (maybe 0 (1+))
