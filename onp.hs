import Data.List

-- Transform the algebraic expression with brackets into RPN form (Reverse Polish Notation). 
-- Two-argument operators: +, -, *, /, ^ (priority from the lowest to the highest), brackets ( ). 
-- Operands: only letters: a,b,...,z. 
-- Assume that there is only one RPN form (no expressions like a*b*c). 

data Tree a = Node a (Tree a) (Tree a) | Leaf deriving (Show,Eq)

oNPtraversal :: Tree String -> String 
oNPtraversal Leaf = ""
oNPtraversal (Node x a b)  = (oNPtraversal a) ++ (oNPtraversal b) ++ x


foldbreak :: ([String], String,Int) -> Char -> ([String], String,Int)
foldbreak (acc, start ,counter) x = case x of '(' -> (acc, start ++ [x] , counter+1) 
                                              ')' -> if counter == 1
                                                         then (acc ++ [start ++ [x]],"" ,0)
                                                         else (acc, start ++ [x], counter-1) 
                                              
                                              _ -> if counter == 0 
                                                         then  (acc ++ [start ++ [x]],"" ,0) 
                                                         else (acc, start ++ [x] , counter)
break_ :: String -> [String]
break_ x = let meat = init . tail $ x 
               (a,j,y) = foldl foldbreak ([],"",0) meat 
           in a


stringToONP :: String -> Tree String
stringToONP k  
  | length k == 1 = Node k Leaf Leaf
  | otherwise = let [operand1, operator, operand2] = break_ k
                in Node operator (stringToONP operand1) (stringToONP operand2)


mainFunc :: String -> String
mainFunc = oNPtraversal . stringToONP
                    
                    

