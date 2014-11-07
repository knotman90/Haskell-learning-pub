module MyList where

infixr 5 :-:
data List a= Empty |  a :-: (List  a) deriving (Show, Read, Eq, Ord)

infixr 5  .++  
(.++) :: List a -> List a -> List a   
Empty .++ ys = ys  
(x :-: xs) .++ ys = x :-: (xs .++ ys) 

myListFromList :: [a] -> List a
myListFromList [] = Empty
myListFromList (x:xs) = x :-: myListFromList xs

myReverse :: List a -> List a
myReverse Empty = Empty
myReverse (x:-: Empty) = x :-: Empty
myReverse (x:-:xs) =  (myReverse xs)  .++ (x :-: Empty)


myMap :: (a -> a) -> List a -> List a
myMap _ Empty = Empty
myMap f (x:-:xs) = (f x) :-: myMap f xs 

myFoldl :: (a -> a -> a) ->a-> List a ->  a
myFoldl _ a Empty = a 
myFoldl f a (x:-:xs) =  myFoldl f (f a x) xs


myFoldr :: (a -> a -> a) ->a-> List a ->  a
myFoldr _ a Empty = a
myFoldr _ a (x:-:Empty) = x  
myFoldr f a (x:-:xs) =  f (myFoldr f a xs) x
