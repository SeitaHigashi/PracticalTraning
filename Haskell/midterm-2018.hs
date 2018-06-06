change_case :: [Char] -> [Char]
change_case s = [ if c >= 'a' && c <= 'z' then toEnum ((fromEnum c)-32) :: Char else toEnum ((fromEnum c)+32) :: Char | c <- s]
pythagoras :: Integer -> [(Integer,Integer,Integer)]
pythagoras n = [(a,b,c)|c<-[3..n],b<-[2..c],a<-[1..b],a^2+b^2 == c^2,(gcd a b) == 1]
second xs = head (tail xs)
third xs = head (tail (tail xs))
comb n k =  (product [(n-k+1)..n]) `div` (product [1..k])
squaringcomb n k = sum [(comb n a)^2| a <- [0..n]]
squaringcomb2 n k = sum [(comb n (2*a))^2| a <- [0..(n `div` 2)]]
