module Party where
import Employee
import Data.Tree

glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp _ fun) (GL es f) = GL (e:es) (f + fun)

instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL as fa) (GL bs fb) = GL (as ++ bs) (fa + fb)

moreFun :: GuestList -> GuestList -> GuestList
moreFun a b = if (a > b) then a else b

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node x ss) = f x (map (treeFold f) ss)

test :: Tree Employee
test
    = Node (Emp "A" 12)
      [ Node (Emp "B" 10) []
      , Node (Emp "C" 10) []
      ]

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss@(Emp _ bFun) [] = (GL [boss] bFun, GL [] 0)
nextLevel boss subs = (moreFun with without, without)
    where
        subWithouts = map snd subs
        subWiths = map fst subs
        with = glCons boss (mconcat subWithouts)
        without = mconcat subWiths

maxFun :: Tree Employee -> GuestList
maxFun = fst . treeFold nextLevel
