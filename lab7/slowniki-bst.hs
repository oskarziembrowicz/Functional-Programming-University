import Distribution.Compat.Lens (set)
 -- Dict k v 

data Dict k v = E | N k v (Dict k v) (Dict k v)
    deriving Show

emptyTree :: Dict k v
emptyTree = E

listToSet :: Ord k => [(k, v)] -> Dict k v
listToSet = foldl (\acc (k,v) -> setNode acc k v) E

setNode :: Ord k => Dict k v -> k -> v -> Dict k v
setNode E key value = N key value E E
setNode (N k v l r) key value
    | k == key = N key value l r
    | key < k = N k v (setNode l key value) r
    | otherwise = N k v l (setNode r key value)

getValue :: Ord k => Dict k v -> k -> Maybe v
getValue E _ = Nothing
getValue (N k v l r) key
    | k == key = Just v
    | key < k = getValue l key
    | otherwise = getValue r key

keys :: Dict k v -> [k]
keys E = []
keys (N k _ l r) = keys l ++ [k] ++ keys r

values :: Dict k v -> [v]
values E = []
values (N _ v l r) = values l ++ [v] ++ values r