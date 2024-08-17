module Graph where
import Set(Set, toAscList)
import qualified Set as Set
import Data.List (sort)
import Data.List.NonEmpty (fromList)
class Graph g where
  empty   :: g a
  vertex  :: a -> g a
  union   :: g a -> g a -> g a
  connect :: g a -> g a -> g a

data Relation a = Relation { domain :: Set a, relation :: Set (a, a) }
    deriving (Eq, Show)

data Basic a = Empty
             | Vertex a
             | Union (Basic a) (Basic a)
             | Connect (Basic a) (Basic a)

unionDomains l r = Set.union (domain l) (domain r)
unionRelations l r = Set.union (relation l) (relation r)

instance Graph Relation where
  empty = Relation Set.empty Set.empty
  vertex elem = Relation (Set.singleton elem) Set.empty
  union l r = Relation (unionDomains l r) (unionRelations l r)
  connect l r = Relation (unionDomains l r)
    $ Set.fromList ([(v1, v2) | v1 <- Set.toList (domain l), v2 <- Set.toList (domain r) ] ++ Set.toList (unionRelations l r))

instance (Ord a, Num a) => Num (Relation a) where
    fromInteger = vertex . fromInteger
    a + b       = Relation (Set.fromList (toAscList (domain c))) (Set.fromList (toAscList (relation c)))
      where c = union a b
    a * b       = Relation (Set.fromList (toAscList (domain c))) (Set.fromList (toAscList (relation c)))
      where c = connect a b
    signum      = const empty
    abs         = id
    negate      = id

instance Graph Basic where
  empty = Empty
  vertex = Vertex
  union = Union
  connect = Connect

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

vertices :: Ord a => Basic a -> [a]
vertices g = sortUnique (_vertices g []) where
  _vertices Empty acc = acc
  _vertices (Vertex elem) acc = elem:acc
  _vertices (Union l r) acc = _vertices r (_vertices l acc)
  _vertices (Connect l r) acc = _vertices r (_vertices l acc)

disconnectedVertices :: Ord a => Basic a -> [a]
disconnectedVertices g = sortUnique (_dis_vertices g []) where
  _dis_vertices Empty acc = acc
  _dis_vertices (Vertex elem) acc = elem:acc
  _dis_vertices (Union l r) acc = _dis_vertices r (_dis_vertices l acc)
  _dis_vertices (Connect l r) acc = acc

edges :: Ord b => Basic b -> [(b, b)]
edges g = sortUnique (_edges g []) where
  _edges Empty acc = acc
  _edges (Vertex elem) acc = acc
  _edges (Union l r) acc = _edges r (_edges l acc)
  _edges (Connect l r) acc = [(i, j) | i <- vertices l, j <- vertices r] ++ _edges r (_edges l acc)

instance Ord a => Eq (Basic a) where
  left == right = (vertices left == vertices right) && (edges left == edges right)

instance (Ord a, Num a) => Num (Basic a) where
    fromInteger = vertex . fromInteger
    (+)         = union
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id

instance Semigroup (Basic a) where
  (<>) = union

instance Monoid (Basic a) where
  mempty = Empty

fromBasic :: Graph g => Basic a -> g a
fromBasic Empty = empty
fromBasic (Vertex a) = vertex a
fromBasic (Union l r) = union (fromBasic l) (fromBasic r)
fromBasic (Connect l r) = connect (fromBasic l) (fromBasic r)


instance (Ord a, Show a) => Show (Basic a) where
  show g = "edges " ++ show (edges g) ++ " + vertices " ++ show (disconnectedVertices g)

-- | Example graph
-- >>> example34
-- edges [(1,2),(2,3),(2,4),(3,5),(4,5)] + vertices [17]

example34 :: Basic Int
example34 = 1*2 + 2*(3+4) + (3+4)*5 + 17

-- >>> (1 * 2 + 2 * 1) ::Basic Int
-- edges [(1,2),(2,1)] + vertices []


showConcList :: Foldable t => t [a] -> [a] -> [a]
showConcList l s = foldr (++) s l
-- | showConcLists prepends strings from list "l" to string "s"                        
-- >>> showConcList ["Ala ", "ma ", "kota"] "!!!"
-- "Ala ma kota!!!"

todot :: (Ord a, Show a) => Basic a -> String
todot g =
  showString "digraph {\n" .
  showConcList [show a ++ " -> " ++ show b ++ ";\n" | (a,b) <- edges g] .
  showConcList [show a ++ ";\n" | a <- disconnectedVertices g] $ "}"
-- >>> todot example34
-- "digraph {\n1 -> 2;\n2 -> 3;\n2 -> 4;\n3 -> 5;\n4 -> 5;\n17;\n}"


instance Functor Basic where
  fmap f Empty = Empty
  fmap f (Vertex a) = Vertex (f a)
  fmap f (Union l r) = Union (fmap f l) (fmap f r)
  fmap f (Connect l r) = Connect (fmap f l) (fmap f r)


-- | Merge vertices
-- >>> mergeV 3 4 34 example34
-- edges [(1,2),(2,34),(34,5)] + vertices [17]

mergeV :: Eq a => a -> a -> a -> Basic a -> Basic a
mergeV a b c = fmap f
  where
    f x = if x == a || x == b
      then c
      else x

instance Applicative Basic where  
  (<*>) funs_graph val_graph = do
    f <- funs_graph
    a <- val_graph
    return (f a)
  pure = Vertex


instance Monad Basic where
  Empty >>= f = Empty
  Vertex x >>= f = f x
  Union l r >>= f = Union (l >>= f) (r >>= f)
  Connect l r >>= f = Connect (l >>= f) (r >>= f)


-- | Split Vertex
-- >>> splitV 34 3 4 (mergeV 3 4 34 example34)
-- edges [(1,2),(2,3),(2,4),(3,5),(4,5)] + vertices [17]


splitV :: Eq a => a -> a -> a -> Basic a -> Basic a
-- splitV a b c graph = graph >>= \x -> if x == a then Union (pure b) (pure c) else pure x 
splitV a b c graph = do
  x <- graph
  if x == a
    then Union (Vertex b) (Vertex c)
    else return x
