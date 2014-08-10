import Data.Monoid
import Sized

data JoinList m a = Empty | Single m a | Append m (JoinList m a) (JoinList m a) deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty l     = l
(+++) l Empty     = l
(+++) l1 l2 = Append (mappend (tag l1) (tag l2)) l1 l2

uSize :: (Sized a) => a -> Int
uSize = getSize . size

listSize :: (Sized a, Monoid a) => JoinList a b -> Int
listSize = uSize . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i (Single _ v)
  | i == 0    = Just v
  | otherwise = Nothing
indexJ i (Append s l1 l2)
  | i < (listSize l1) = indexJ i l1
  | i < (listSize l1) + (listSize l2) = indexJ (i - (listSize l1)) l2
  | otherwise = Nothing

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ i l@(Single _ _)
  | i >= 1    = Empty
  | otherwise = l
dropJ i (Append s l1 l2)
  | i > uSize s     = Empty
  | i > listSize l1 = dropJ (i - listSize l1) l2
  | otherwise       = Append (Size (uSize s - i)) (dropJ i l1) l2

indexToList :: JoinList b a -> [a]
indexToList Empty = []
indexToList (Single _ v) = [v]
indexToList (Append _ l1 l2) = (indexToList l1) ++ (indexToList l2)


list :: JoinList Size Char
list = Append (Size 3) (Append (Size 2) (Single (Size 1) 'a') (Single (Size 1) 'b')) (Single (Size 1) 'd')

main = putStrLn $ show $ indexJ 1 list
