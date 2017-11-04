splitNum :: Int -> (Int, Int)
splitNum x = let stringForm = show x
                 k = length stringForm
                 length_ = (div k 2) + (rem k 2)
             in (k - length_, read (take length_ stringForm))

formPalindrome :: (Int,Int) -> Int
formPalindrome (0,x) = x+1 
formPalindrome (y,x) = let stringForm = show x 
                           l = length stringForm 
                           part = take y stringForm 
                           lasti = drop y stringForm
                           in read (part ++ lasti ++ (reverse part))


solution :: Int -> Int
solution x = let list_ = iterate (fmap (1+)) (splitNum x) 
                 aa = map formPalindrome list_
             in (head . (dropWhile (\y -> y <= x))) $ aa



