
-- #1
multiple3_or_5 :: (Integral a, Eq a) => a-> Bool
multiple3_or_5 n 
	| (mod n 3 ==0) || (mod n 5 ==0) = True
	| otherwise = False

listAllMultiple_3_or_5 ::(Integral a, Eq a) => [a]->[a]
listAllMultiple_3_or_5 xs = [a | a<-xs , multiple3_or_5 a]


{-| 
	#2 n! means n × (n − 1) × ... × 3 × 2 × 1

	For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
	and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

	Find the sum of the digits in the number 100!
|-}
fac :: (Integral a, Eq a) => a->a
fac 0 = 0
fac 1 = 1
fac n = n* fac (n-1)

intToList' :: (Integral a ,Num a) => a ->[a]
intToList' 0 = []
intToList' n =  n `mod` 10 : intToList' (n `quot` 10) -- reverse digits list

intToList n = reverse (intToList' n)


sumUp :: (Num a) => [a] -> a
sumUp [] = 0
sumUp (x:xs) = x + sumUp xs 


{-| 
	In England the currency is made up of pound, £, and pence, p, and there are eight coins in general circulation:

	1p,  kp, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
	It is possible to make £2 in the following way:

	1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
	How many different ways can £2 be made using any number of coins?

combos :: Int -> [a] ->[[a]]
combos 0 _ = [[]]
combos _ [] = [[]]
combos k (x:xs) = x_start ++ others
	where  x_start = [x:rest | rest <- combos (k-1) xs]
	       others = if k<=length xs then combos k xs else  []

money x = [y | y<-[x..]
moneyCombos :: (Num a , Eq a)=> Int ->[[a]]
moneyCombos n = [x| x<-combos n [1,2,5,10,20,50,100,200], (sumUp x)==200]
|-}

perm :: Int -> [a] ->[[a]]
perm 0 _ = [[]]
perm _ [] = [[]]
perm 1 xs = [(x:[]) | x<-xs]
perm n l@(x:xs) = [k | (y:c)<-(perm 1 l), k <-(map (y:) (perm (n-1) l)) ]
--perm n l@(x:xs)=  (map (x:) (perm (n-1) l))

dotProduct ::(Num a)=> [a]->[a] -> a
dotProduct _ [] = 0
dotProduct [] _ = 0
dotProduct (x:xs) (y:ys) = (x*y) + dotProduct xs ys



takeRotations :: (Num a,Ord a)=> [a]->[[a]]
takeRotations xs = [k | k@(a:b)<-(cycleK (length xs) ((take(2*(length xs)-1) (cycle xs)))), a/=0  ]

fromListToNumber ::  [Int]-> Int
fromListToNumber [] = 0
fromListToNumber (x:xs) = (x*(10^(length xs))) + (fromListToNumber xs)
fromListToNumbers :: [[Int]]-> [Int]
fromListToNumbers[] = []
fromListToNumbers xs= [fromListToNumber k | k<-xs ]

cycleK :: Int->[a]->[[a]]
cycleK _ [] = [[]]
cycleK n xs 
	| ((length xs) >= n) = [(take n xs)] ++ (cycleK n (drop 1 xs)) 
	|otherwise = [[]]

isPrime' ::(Integral a)=> a-> a-> Bool
isPrime' _ 1 = True
isPrime' n k = ((n `mod` k) /= 0) && (isPrime' n (k-1))
 
isPrime ::(Integral a) => a-> Bool
isPrime n = isPrime' n (n `div` 2)

find x [] = False
find x (l:ls)
	| x == l = True
	| otherwise = find x ls

allPrime = [k | k <- [2..1000], isPrime k]
oddNotPrime = [k | k <- [2..1000], (find k allPrime) == False, (k `mod` 2) == 1 ]

-- odd = prime + 2*quadrato


