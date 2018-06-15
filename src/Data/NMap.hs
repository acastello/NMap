{-# LANGUAGE CPP #-}

module Data.NMap (
    NMap, leaf, roots, (.<), (.>), drawNMap, mapKeys, mapWithKey, mapWithKey0,
    traverseKeys, traverseWithKey, bitraverse, bisequence, mapKeysM, fromList, 
    elems, delete, delete0, rootKeys, lookup, (!), member, branch, branch0, 
    insert, insert0, fromKeys, toKeys
    )where

import qualified Data.Map as M
import Data.Bifunctor
import Data.List (intersperse)
#if MIN_VERSION_base(4,9,0)
import Data.Semigroup
#endif
import Data.Tuple (swap)
import Prelude hiding (lookup)

data NMap k a =
      Branch { roots :: M.Map k (NMap k a) }
    | Leaf { leaf :: a }
    deriving Eq

infixr 0 .<
(.<) :: Ord k => a -> [(k, NMap k b)] -> (a, NMap k b)
k .< xs = (k, Branch $ M.fromList xs)

infixr 0 .>
(.>) :: k -> a -> (k, NMap k a)
k .> a = (k, Leaf a)

instance (Show a, Show b) => Show (NMap a b) where
    show (Leaf a) = concat ["[", show a, "]"]
    show (Branch m) = concat $ ["["] ++ (intersperse ", " $ show' <$> (M.toList m)) ++ ["]"]
        where show' (k,a) = show k ++ " -> " ++ show a

drawNMap :: (Show k, Show a) => NMap k a -> String
drawNMap mt = unlines $ firstdraw mt where
    firstdraw (Leaf a) = concat ["{", show a, "}"] :[]
    firstdraw (Branch m) = firstdraw' (M.toList m)
    firstdraw' [] = ["()"]
    firstdraw' [(x,m)] = shift (show x ++ "─") (show x ++ " " *> " ") (draw m)
    firstdraw' ((x,m):xs) = (shift ("┌─" ++ show x ++ "─") ("│ " ++ (show x ++ "-" *> " ")) (draw m)) ++ (firstdraw'' xs)
    firstdraw'' [] = undefined
    firstdraw'' [(x,m)] = shift ("└─" ++ show x ++ "─") ("└─" ++ show x ++ "─" *> " ") (draw m)
    firstdraw'' ((x,m):xs) = shift ("├─" ++ show x ++ "─") ("│ " ++ (show x ++ " " *> " ")) (draw m) ++ (firstdraw'' xs)
    draw (Leaf a) = concat ["──{", show a, "}"] :[]
    draw (Branch m) = draw' $ M.toList m
    draw' [] = "()":[]
    draw' [(x,m)] = shift ("──" ++ show x ++ "─") ("--" ++ show x ++ "-" *> " ") (draw m)
    draw' ((x,m):xs) = shift ("┬─" ++ show x ++ "─") ("│ " ++ (show x ++ " " *> " ")) (draw m) ++ (draw'' xs)
    draw'' [] = undefined
    draw'' [(x,m)] = shift ("└─" ++ show x ++ "─") ("  " ++ show x ++ " " *> " ") (draw m) 
    draw'' ((x,m):xs) = shift ("├─" ++ show x ++ "─") ("│ " ++ (show x ++ " " *> " ")) (draw m) ++ (draw'' xs)
    shift lhs other = zipWith (++) (lhs : repeat other)

#if MIN_VERSION_base(4,9,0)
instance Ord k => Semigroup (NMap k b) where
    (<>) = mappend
    stimes = stimesIdempotentMonoid
#endif

instance Ord k => Monoid (NMap k b) where
    mempty = Branch M.empty
    (Branch m1) `mappend` (Branch m2) = Branch $ M.unionWith mappend m1 m2
    lleaf `mappend` _ = lleaf

instance Functor (NMap k) where
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Branch m) = Branch $ (fmap f) <$> m
    
instance Applicative (NMap k) where
    pure a = Leaf a
    Leaf f <*> t = f <$> t       
    Branch m <*> Leaf a = ($ a) <$> Branch m
    Branch m <*> Branch m' = Branch $ (Branch m <*>) <$> m'
    
instance Monad (NMap k) where
    return = pure
    Leaf a >>= f = f a
    Branch m >>= f = Branch $ (>>= f) <$> m
    
    
mapKeys :: Ord k2 => (k1 -> k2) -> NMap k1 a -> NMap k2 a
mapKeys _ (Leaf a) = Leaf a
mapKeys f (Branch m) = Branch $ (mapKeys f) <$> (M.mapKeys f m)

mapWithKey :: (k -> a -> b) -> NMap k a -> NMap k b
mapWithKey _ (Leaf _) = error "can't map without a key"
mapWithKey f (Branch m) = Branch $ M.mapWithKey (\k mt -> mapWithKey0 f k mt) m

mapWithKey0 :: (k -> a -> b) -> k -> NMap k a -> NMap k b
mapWithKey0 f k (Leaf a) = Leaf (f k a)
mapWithKey0 f _ (Branch m) = Branch $ M.mapWithKey (\k' mt -> mapWithKey0 f k' mt) m

mapWithKeys :: ([k] -> a -> b) -> NMap k a -> NMap k b
mapWithKeys f m = mapWithKeys0 f [] m
    where
        mapWithKeys0 f' ks (Leaf a) = Leaf $ f' ks a
        mapWithKeys0 f' ks (Branch m') = Branch $ M.mapWithKey (\k mt -> mapWithKeys0 f' (ks ++ [k]) mt) m'

instance Foldable (NMap k) where
    foldr = fold

fold :: (a -> b -> b) -> b -> NMap k a -> b
fold f b (Leaf a) = f a b
fold f b (Branch m) = foldr (flip $ fold f) b m

instance Traversable (NMap k) where
    traverse f (Leaf a) = Leaf <$> f a    
    traverse f (Branch m) = Branch <$> (traverse (traverse f) m)

traverseKeys :: (Ord k2, Applicative t) => (k1 -> t k2) -> NMap k1 a -> t (NMap k2 a)
traverseKeys f = bitraverse f pure

traverseWithKey :: (Ord k, Applicative t) => (k -> a -> t b) -> NMap k a -> t (NMap k b)
traverseWithKey f = sequenceA . (mapWithKey f)

bitraverse :: (Applicative t, Ord k2) => (k1 -> t k2) -> (a1 -> t a2) -> NMap k1 a1 -> t (NMap k2 a2)
bitraverse _ g (Leaf a) = Leaf <$> g a
bitraverse f g (Branch m) = Branch <$> (traverseK2 f (bitraverse f g) m)

traverseK2 :: (Applicative t, Ord k2) => (k1 -> t k2) -> (a -> t b) -> M.Map k1 a -> t (M.Map k2 b)
traverseK2 f g = (M.fromList <$>) . (traverse (\(x,y) -> (,) <$> f x <*> g y)) . M.toList

bisequence :: (Applicative t, Ord k) => NMap (t k) (t a) -> t (NMap k a)
bisequence = bitraverse id id

mapKeysM :: (Ord k2, Monad m) => (k1 -> m k2) -> NMap k1 a -> m (NMap k2 a)
mapKeysM = traverseKeys

fromList :: Ord k => [(k,NMap k a)] -> NMap k a
fromList = Branch . M.fromList

fromKeys :: Ord k => [([k], a)] -> NMap k a
fromKeys xs = foldMap (uncurry branch) xs

toKeys :: Ord k => NMap k a -> [([k], a)]
toKeys = elems . (mapWithKeys (,))

elems :: NMap k a -> [a]
elems = foldr (:) []

-- alter :: (Maybe a -> Maybe a) -> [k] -> NMap k a -> NMap k a
-- alter f ks m = lookup 

delete :: Ord k => [k] -> NMap k a -> NMap k a
delete (k:[]) (Branch m) = Branch $ M.delete k m
delete (k:ks) (Branch m) = Branch $ M.adjust (delete ks) k m
delete [] nm = nm
delete _ l = l

delete0 :: Ord k => k -> NMap k a -> NMap k a
delete0 k (Branch m) = Branch $ M.delete k m
delete0 _ l = l

rootKeys :: NMap k a -> [k]
rootKeys (Leaf _) = error "0-depth trees don't have keys"
rootKeys (Branch m) = M.keys m

lookup :: Ord k => k -> NMap k a -> Maybe (Either (NMap k a) a)
lookup _ (Leaf _) = Nothing
lookup k (Branch m) = case M.lookup k m of
    Just (Branch m') -> Just (Left $ Branch m')
    Just (Leaf a) -> Just (Right a)
    Nothing -> Nothing

(!) :: Ord k => NMap k a -> [k] -> a
(Leaf a)![] = a
(Leaf _)!_ = error "too many keys provided"
(Branch m)!(k:ks) = case m M.! k of
    (Leaf a') -> a'
    b -> b!ks
(Branch _)![] = error "not enough keys"
    

member :: Ord k => k -> NMap k a -> Bool
member k (Branch m) = M.member k m
member _ _ = False

branch :: Ord k => [k] -> a -> NMap k a
branch ks e = foldr (\a b -> fromList [(a,b)]) (Leaf e) ks

branch0 :: Ord k => k -> a -> NMap k a
branch0 k e = fromList [k .> e]

insert :: Ord k => [k] -> a -> NMap k a -> NMap k a
insert ks e m = (branch ks e) `mappend` m

insert0 :: Ord k => k -> a -> NMap k a -> NMap k a 
insert0 k e m = insert [k] e m

-- test1 = fromList
--     [ 1 .<
--         [ 1 .> 'a'
--         , 2 .< 
--             [ 1 .> 'b'
--             , 2 .> 'c']
--         , 3 .< 
--             [ 4 .> 'd' ] ]
--     , 2 .> 'e' ]
--         
-- test2 = fromList
--     [ 1.< [ 2 .> 'a' ]]
-- 
-- test3 = fromList
--     [ 1.< 
--         [ -2 .> 'a' ]
--     , 3 .> 'b'
--     , 4 .< [] ] 
-- 
-- test4 = fromList
--     [ 1 .> (:[]) ]
-- 
-- test5 = fromList
--     [ -1 .> (:[])
--     , -2 .> (:"-d")]
