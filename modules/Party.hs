module Party where
import Employee
import Data.Tree

-- -- Can be safely used in a situation where GuestLists are guaranteed to behave "additively"
glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp _ fun) (GL es f) = GL (e:es) (f + fun)

-- Can be safely used in a situation where GuestLists are guaranteed to behave "additively"
instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL as fa) (GL bs fb) = GL (as ++ bs) (fa + fb)

moreFun :: GuestList -> GuestList -> GuestList
moreFun a b = if (a > b) then a else b

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node x ss) = f x (map (treeFold f) ss)

-- Consider an employee node, and the result pair for each subordinate employee.
-- The first GuestList for each subordinate is the best GuestList including that Employee
-- The second GuestList for each subordinate is the best GuestList NOT including that Employee
-- Based on this information, the function constructs the the best GuestList including this Employee,
-- and the best GuestList NOT including this Employee.
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss@(Emp _ bFun) [] = (GL [boss] bFun, GL [] 0)
-- Pick the best of each category -- we have two choices for best-with-this-employee, and only one
-- for best-without-this-employee. In the case with two choices, pick the one that's more fun.
nextLevel boss subs = (moreFun with without, without)
    where
        subWithouts = map snd subs -- The best GuestLists for each Employee NOT including that Employee
        subWiths = map fst subs -- The best GuestLists for each Employee including that Employee
        -- The subWithouts safely behave additively, since throwing them together will not result in
        -- anyone's direct boss being added to the mix. Furthermore, the addition of this Employee will
        -- is also additive because he is two authority levels removed from everyone in the subWithouts
        with = glCons boss (mconcat subWithouts)
        -- If we don't include this employee, we can throw together all the best results including the
        -- the Employees under him, and these will safely behave additively.
        without = mconcat subWiths

maxFun :: Tree Employee -> GuestList
maxFun = fst . treeFold nextLevel
