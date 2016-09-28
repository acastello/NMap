module Data.MapTree (
    MapTree, (.<), (.>), drawMapTree, mapKeys, mapWithKey, mapWithKey0,
    traverseKeys, mapKeysM, fromList, elems, baseKeys, lookup, (!), member,
    branch, insert, insert0         
    )where

import qualified Data.Map as M
import Data.List (intersperse)
import Data.Tuple (swap)
import Prelude hiding (lookup)

data MapTree k a =
      Branch { roots :: M.Map k (MapTree k a) }
    | Leaf { leaf :: a }
    deriving Eq

infixr 0 .<
k .< xs = (k, Branch $ M.fromList xs)

infixr 0 .>
k .> a = (k, Leaf a)

instance (Show a, Show b) => Show (MapTree a b) where
    show (Leaf a) = concat ["[", show a, "]"]
    show (Branch m) = concat $ ["["] ++ (intersperse ", " $ show' <$> (M.toList m)) ++ ["]"]
        where show' (k,a) = show k ++ " -> " ++ show a

drawMapTree :: (Show k, Show a) => MapTree k a -> String
drawMapTree mt = unlines $ firstdraw mt where
    firstdraw (Leaf a) = concat ["{", show a, "}"] :[]
    firstdraw (Branch m) = firstdraw' (M.toList m)
    firstdraw' [] = ["()"]
    firstdraw' [(x,m)] = shift (show x ++ "─") (show x ++ " " *> " ") (draw m)
    firstdraw' ((x,m):xs) = (shift ("┌─" ++ show x ++ "─") ("│ " ++ (show x ++ "-" *> " ")) (draw m)) ++ (firstdraw'' xs)
    firstdraw'' [(x,m)] = shift ("└─" ++ show x ++ "─") ("└─" ++ show x ++ "─" *> " ") (draw m)
    firstdraw'' ((x,m):xs) = shift ("├─" ++ show x ++ "─") ("│ " ++ (show x ++ " " *> " ")) (draw m) ++ (firstdraw'' xs)
    draw (Leaf a) = concat ["──{", show a, "}"] :[]
    draw (Branch m) = draw' $ M.toList m
    draw' [] = "()":[]
    draw' [(x,m)] = shift ("──" ++ show x ++ "─") ("--" ++ show x ++ "-" *> " ") (draw m)
    draw' ((x,m):xs) = shift ("┬─" ++ show x ++ "─") ("│ " ++ (show x ++ " " *> " ")) (draw m) ++ (draw'' xs)
    draw'' [(x,m)] = shift ("└─" ++ show x ++ "─") ("  " ++ show x ++ " " *> " ") (draw m) 
    draw'' ((x,m):xs) = shift ("├─" ++ show x ++ "─") ("│ " ++ (show x ++ " " *> " ")) (draw m) ++ (draw'' xs)
    shift first other = zipWith (++) (first : repeat other)

instance Ord k => Monoid (MapTree k b) where
    mempty = Branch M.empty
    (Branch m1) `mappend` (Branch m2) = Branch $ M.unionWith mappend m1 m2
    fst `mappend` _ = fst

instance Functor (MapTree k) where
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Branch m) = Branch $ (fmap f) <$> m
    
instance Applicative (MapTree k) where
    pure a = Leaf a
    Leaf f <*> t = f <$> t       
    Branch m <*> Leaf a = ($ a) <$> Branch m
    Branch m <*> Branch m' = Branch $ (Branch m <*>) <$> m'
    
instance Monad (MapTree k) where
    return = pure
    Leaf a >>= f = f a
    Branch m >>= f = Branch $ (>>= f) <$> m
    
    
mapKeys :: Ord k2 => (k1 -> k2) -> MapTree k1 a -> MapTree k2 a
mapKeys f (Leaf a) = Leaf a
mapKeys f (Branch m) = Branch $ (mapKeys f) <$> (M.mapKeys f m)

mapWithKey :: (k -> a -> b) -> MapTree k a -> MapTree k b
mapWithKey _ (Leaf _) = error "can't map without a key"
mapWithKey f (Branch m) = Branch $ M.mapWithKey (\k mt -> mapWithKey0 f k mt) m

mapWithKey0 :: (k -> a -> b) -> k -> MapTree k a -> MapTree k b
mapWithKey0 f k (Leaf a) = Leaf (f k a)
mapWithKey0 f k (Branch m) = Branch $ M.mapWithKey (\k' mt -> mapWithKey0 f k' mt) m

instance Foldable (MapTree k) where
    foldr = fold

fold :: (a -> b -> b) -> b -> MapTree k a -> b
fold f b (Leaf a) = f a b
fold f b (Branch m) = foldr (flip $ fold f) b m

instance Traversable (MapTree k) where
    traverse f (Leaf a) = Leaf <$> f a    
    traverse f (Branch m) = Branch <$> (traverse (traverse f) m)

traverseKeys :: (Ord k2, Applicative t) => (k1 -> t k2) -> MapTree k1 a -> t (MapTree k2 a)
traverseKeys f (Leaf a) = pure (Leaf a)
traverseKeys f (Branch m) = Branch <$> (traverseK2 f (traverseKeys f) m)

mapKeysM :: (Ord k2, Monad m) => (k1 -> m k2) -> MapTree k1 a -> m (MapTree k2 a)
mapKeysM = traverseKeys

traverseK2 :: (Applicative t, Ord k2) => (k1 -> t k2) -> (a -> t b) -> M.Map k1 a -> t (M.Map k2 b)
traverseK2 f g = (M.fromList <$>) . (traverse (\(x,y) -> (,) <$> f x <*> g y)) . M.toList

fromList :: Ord k => [(k,MapTree k a)] -> MapTree k a
fromList = Branch . M.fromList

elems :: MapTree k a -> [a]
elems = foldr (:) []

baseKeys :: MapTree k a -> [k]
baseKeys (Leaf _) = error "0-depth trees don't have keys"
baseKeys (Branch m) = M.keys m

lookup :: Ord k => k -> MapTree k a -> Maybe (Either (MapTree k a) a)
lookup k (Leaf _) = Nothing
lookup k (Branch m) = case M.lookup k m of
    Just (Branch m') -> Just (Left $ Branch m')
    Just (Leaf a) -> Just (Right a)
    Nothing -> Nothing

(!) :: Ord k => MapTree k a -> [k] -> a
(Leaf a)![] = a
(Leaf a)!_ = error "too many keys provided"
(Branch m)!(k:ks) = case m M.! k of
    (Leaf a') -> a'
    b -> b!ks
(Branch m)![] = error "not enough keys"
    

member :: Ord k => k -> MapTree k a -> Bool
member k (Branch m) = M.member k m
member _ _ = False

branch :: Ord k => [k] -> a -> MapTree k a
branch ks a = foldr (\a b -> fromList [(a,b)]) (Leaf a) ks

insert :: Ord k => [k] -> a -> MapTree k a -> MapTree k a
insert ks a m = (branch ks a) `mappend` m

insert0 :: Ord k => k -> a -> MapTree k a -> MapTree k a 
insert0 k a m = insert [k] a m

pt :: (Show k, Show a) => MapTree k a -> IO ()
pt = putStr . drawMapTree

test1 = fromList
    [ 1 .<
        [ 1 .> 'a'
        , 2 .< 
            [ 1 .> 'b'
            , 2 .> 'c']
        , 3 .< 
            [ 4 .> 'd' ] ]
    , 2 .> 'e' ]
        
test2 = fromList
    [ 1.< [ 2 .> 'a' ]]

test3 = fromList
    [ 1.< 
        [ -2 .> 'a' ]
    , 3 .> 'b'
    , 4 .< [] ] 

test4 = fromList
    [ 1 .> (:[]) ]

test5 = fromList
    [ -1 .> (:[])
    , -2 .> (:"-d")]
