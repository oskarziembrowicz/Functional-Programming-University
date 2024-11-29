import Distribution.Simple (VersionInterval)
import GHC.Exts.Heap (GenClosure(key))
type Dict k v = [(k, v)]

emptyDict :: Dict k v 
emptyDict = []

setDict :: Eq k => Dict k v -> k -> v -> Dict k v
setDict [] key value = [(key, value)]
setDict (h:t) key value
    | fst h == key = (key, value) : t
    | otherwise = h : setDict t key value

getDictValue :: Eq k => Dict k v -> k -> Maybe v
getDictValue [] _ = Nothing
getDictValue (h:t) key
    | fst h == key = Just (snd h)
    | otherwise = getDictValue t key

deleteFromDict :: Eq k => Dict k v -> k -> Dict k v
deleteFromDict [] _ = []
deleteFromDict (h:t) key
    | fst h == key = t
    | otherwise = h : deleteFromDict t key

getKeys :: Dict k v -> [k]
getKeys = map fst 

getValues :: Dict k v -> [v]
getValues = map snd