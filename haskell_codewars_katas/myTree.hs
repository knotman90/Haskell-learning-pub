module MyTree where

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq, Ord)

insert :: (Ord a) => a -> Tree a -> Tree a
insert a EmptyTree = Node a (EmptyTree) (EmptyTree)
insert a (Node b t1 t2)
	|a==b = Node b t1 t2
	|a<b = Node b (insert a t1) t2
	|otherwise = Node b t1 (insert a t2) 
