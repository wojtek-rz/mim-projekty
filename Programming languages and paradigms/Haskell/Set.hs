module Set(Set(..), empty, null, singleton, union, fromList
              , member, toList, toAscList, elems, insert
              ) where
import Prelude hiding(null)
import Data.List (sort)

data Set a = Empty
           | Singleton a
           | Union (Set a) (Set a)

instance Foldable Set where
   foldr f z Empty = z
   foldr f z (Singleton a) = f a z
   foldr f z (Union left right) = foldr f (foldr f z right) left

empty :: Set a
empty = Empty

null :: Set a -> Bool
null Empty = True
null _     = False

member :: Eq a => a -> Set a -> Bool
member l Empty = False
member l (Singleton a) = l == a
member l (Union a b) = member l a || member l b

singleton :: a -> Set a
singleton = Singleton

fromList :: [a] -> Set a

fromList list =
  let makeTree list n
        | n == 0 = (Empty, list)
        | n == 1 = (Singleton (head list), tail list)
        | otherwise =
            let (lewe_poddrzewo, reszta) = makeTree list (n `div` 2) in
            let (prawe_poddrzewo, reszta2) = makeTree reszta (n - (n `div `2))
            in (Union lewe_poddrzewo prawe_poddrzewo, reszta2)
  in let (tree, _) = makeTree list (length list)
  in tree

toList :: Set a -> [a]
toList = foldr (:) []

sortUnique :: (Ord a) => [a] -> [a]
sortUnique [] = []
sortUnique list =
  let x:t = sort list in
  x : removeReps x t
  where
    removeReps x [] = []
    removeReps x (h:t)
        | x == h = removeReps x t
        | otherwise = h:removeReps h t

toAscList :: Ord a => Set a -> [a]
toAscList = sortUnique . toList

elems :: Set a -> [a]
elems = toList

union :: Set a -> Set a -> Set a
union = Union

insert :: a -> Set a -> Set a
insert elem g = Union g (Singleton elem)

instance Ord a => Eq (Set a) where
  left == right = toAscList left == toAscList right

instance Semigroup (Set a) where
  (<>) = union

instance Monoid (Set a) where
  mempty = empty

instance Show a => Show (Set a) where
  show set = show (toList set)

instance Functor Set where
  fmap f Empty = Empty
  fmap f (Singleton l) = Singleton (f l)
  fmap f (Union left right) = Union (fmap f left) (fmap f right)
