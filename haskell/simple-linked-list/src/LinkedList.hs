module LinkedList
    ( LinkedList
    , datum
    , fromList
    , isNil
    , new
    , next
    , nil
    , reverseLinkedList
    , toList
    ) where

data LinkedList a = Nil | LinkedList { datum :: a
                               , next :: LinkedList a
                               } deriving (Eq, Show)

fromList :: [a] -> LinkedList a
fromList [] = Nil
fromList xs = foldr LinkedList Nil xs

isNil :: LinkedList a -> Bool
isNil Nil = True
isNil _ = False

new :: a -> LinkedList a -> LinkedList a
new = LinkedList

nil :: LinkedList a
nil = Nil

reverse' :: LinkedList a -> LinkedList a -> LinkedList a
reverse' Nil prev = prev
reverse' curr prev = reverse' (next curr) $ LinkedList (datum curr) prev

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList ll = reverse' ll Nil

toList :: LinkedList a -> [a]
toList Nil = []
toList ll = datum ll : toList (next ll)
