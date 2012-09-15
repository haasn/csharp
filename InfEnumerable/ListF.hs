import Prelude hiding (enumFrom)
import Control.Monad.Free
import Data.Void

type ListF a = Free ((,) a)

enumFrom :: Enum a => a -> ListF a f
enumFrom n = Free (n, enumFrom (succ n))

printForever :: Show a => ListF a Void -> IO f
printForever (Free (x, xs)) = print x >> printForever xs

main = printForever (enumFrom 5)
