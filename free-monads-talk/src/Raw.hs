module Raw where

import Control.Monad.Free

data HanoiF x = Move Peg Peg x

instance Functor HanoiF where
  fmap f (Move a b next) = Move a b (f next)

type Hanoi = Free HanoiF
type Peg = Int


move :: Peg -> Peg -> Hanoi ()
move a b = liftF $ Move a b ()

solve :: ( Num a, Eq a) => a -> Peg -> Peg -> Peg -> Hanoi ()
solve 1 a b _ = move a b
solve n a b c = do
  solve (n-1) a c b
  move a b
  solve (n-1) c b a

run :: Free HanoiF a -> IO a
run (Pure r) = return r
run (Free (Move a b next)) = print (a,b) >> run next


runArray :: Free HanoiF a -> [(Peg,Peg)]
runArray (Pure _) = []
runArray (Free (Move a b next)) = (a,b) : runArray next

main :: IO ()
main = print . (iterM runArray) $ solve 3 1 3 2
