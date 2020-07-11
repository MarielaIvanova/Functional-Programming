project calculator-final


import Data.Char
import Prelude hiding (newstring)
data Tree = Leaf| BuildTree String Tree Tree deriving (Eq,Show)


isLeaf :: Tree -> Bool
isLeaf (BuildTree _ x y) = if x == Leaf && y == Leaf then True else False

isOp :: String -> Bool
isOp a = a == "+" || a== "-"|| a== "*"|| a== "/"|| a== "^"

isnumber :: String -> Bool
isnumber [] = False
isnumber (x:xs) = isDigit x

searchLetter :: String-> [(String,String)] -> [String]
searchLetter x =map snd . filter ((==x).fst)

getSymbol :: String -> String 
getSymbol [] = [] 
getSymbol (x:xs)
    |isDigit x = x : getSymbol xs 
    |isAlpha x = [x]
    |otherwise =[]

stringToNumber :: String -> Int
stringToNumber input =read input :: Int

getRoot :: Tree -> String
getRoot (BuildTree value left right) = value
    
leftTree :: Tree -> Tree
leftTree (BuildTree root left right)= left
    
getRightTree :: Tree -> Tree
getRightTree (BuildTree root left right) = right

getminiexpr :: String -> String 
getminiexpr currexpr = if (take 1 (drop (length (takeWhile ( \y -> y /= '+' && y /= '-' && y /= '(')  currexpr ))  currexpr)) == ['('] then init (takeWhile ( \y -> y /= '(')  currexpr ) else (takeWhile ( \y -> y /= '+' && y /= '-')  currexpr )
    
getbracketsstring :: String -> String
getbracketsstring currexpr = takeWhile ( \y -> y /= ')') currexpr
       
isPriorityOp :: String -> Bool
isPriorityOp op = (op == ['*'] || op == ['/'] || op == ['^'])

getminiexpr2 :: String -> Tree
getminiexpr2  currexpr 
    |(isPriorityOp op) && nextisbracket = (snd (buildTree' bracketsstring Leaf))
    |(isPriorityOp op)                  =  buildTreeFrom2TreesAndOp (snd (buildTree' bracketsstring Leaf)) (snd (buildrecursion stringafterbrackets Leaf)) op 
    |otherwise                          =  (snd (buildTree' bracketsstring Leaf))
     where bracketsstring = getbracketsstring  currexpr 
           stringafterbrackets = getminiexpr (drop (2 + length (getbracketsstring  currexpr))  currexpr)
           op = take 1 (drop (1 + length (getbracketsstring  currexpr))  currexpr)
           nextisbracket = (take 1 (drop (2 + length (getbracketsstring  currexpr))  currexpr)) == ['(']

buildTreeFromStrings :: String -> String -> String -> Tree
buildTreeFromStrings x y z = (BuildTree y left right)
    where left = (BuildTree x Leaf Leaf)
          right = (BuildTree z Leaf Leaf)

buildTreeFrom2TreesAndOp :: Tree -> Tree -> String -> Tree
buildTreeFrom2TreesAndOp left right op = (BuildTree op left right)

buildTreeFromTreeOpNum :: Tree-> String -> String -> Tree
buildTreeFromTreeOpNum left number op = (BuildTree op left (BuildTree number Leaf Leaf))

buildrecursion :: String -> Tree -> (String,Tree)
buildrecursion "" tree = ("Nothing",tree)
buildrecursion  currexpr tree
    |getSymbol  currexpr /= [] && (length (drop (length number1)  currexpr)) == 0 = ("Nothing",(BuildTree number1 Leaf Leaf))
    |getSymbol  currexpr /= [] = buildrecursion newstring1 (buildTreeFromStrings number1 op number2 )
    |getSymbol  currexpr == [] = buildrecursion newstring (buildTreeFromTreeOpNum tree number3 operation) 
        where operation = take 1   currexpr
              number3 = getSymbol (drop 1  currexpr)
              newstring = drop ((length number3)+ 1)  currexpr
              number1 = getSymbol  currexpr
              op =  take 1 (drop (length number1)  currexpr)
              number2 = getSymbol (drop ((length number1) + 1)  currexpr)
              newstring1 = drop ((length number1) + (length number2) + 1)  currexpr


buildTree' :: String -> Tree  -> (String,Tree)
buildTree' "" currenttree =  ("Leaf", currenttree)
buildTree' (x:xs)  currenttree 
    |(isDigit x || isAlpha x) && ( take 1  xs == ['+'] ||  take 1 xs == ['-']) = buildTree' xs (BuildTree [x] Leaf Leaf)
    |isDigit x || isAlpha x               = buildTree' newstring1 (snd (buildrecursion (getminiexpr(x:xs)) Leaf))
    |elem x "("                           = buildTree' newstring3 (getminiexpr2 xs) 
    |elem x "*/^" && ( take 1 xs == "(" ) = buildTree' newstring2 (buildTreeFrom2TreesAndOp currenttree (getminiexpr2 (drop 1 xs)) [x])
    |elem x "+-" && ( take 1 xs == "(" )  = buildTree' newstring2 (buildTreeFrom2TreesAndOp currenttree (getminiexpr2 (drop 1 xs)) [x])
    |elem x "+-" = buildTree' newstring (buildTreeFrom2TreesAndOp currenttree newtree [x])
        where newtree = (snd (buildrecursion (getminiexpr xs) Leaf))
              newstring = drop (length(getminiexpr xs)) xs
              newstring1 = drop (length(getminiexpr (x:xs))) (x:xs)
              newstring2= drop (((length (getbracketsstring  xs)) + (length (getminiexpr (drop (length (getbracketsstring  xs) + 1)  xs)))) + 1) xs
              newstring3 = drop (((length (getbracketsstring  xs)) + (length (getminiexpr (drop (length (getbracketsstring xs) + 1)  xs)))) + 1) xs


buildGrandTree :: String -> Tree
buildGrandTree inp = (snd (buildTree' inp Leaf))

power :: Int -> Int -> Int
power x 0 = x 
power x y = power (x * x) (y-1) 

quot' a b
         | a<b = 0 
         | otherwise = 1 + quot' (a-b) b
  
count :: [(String,String)] -> Tree -> String
count ys (BuildTree op (BuildTree left Leaf Leaf) (BuildTree right Leaf Leaf))
    |op == ['+'] = show (newL + newR)
    |op == ['-'] = show(newL - newR)
    |op == ['*'] = show(newL * newR)
    |op == ['/'] = show (quot' newL newR)
    |op == ['^'] = show (power newL newR)
        where newL = if not (isnumber left) then stringToNumber (head (searchLetter left ys )) else stringToNumber left
              newR = if not (isnumber right) then stringToNumber (head (searchLetter right ys)) else stringToNumber right

countExp :: [(String,String)] -> Tree -> ([(String,String)],Tree)
countExp varlist (BuildTree root l r) 
    | l==Leaf && r == Leaf                          = (varlist, (BuildTree root l r))
    | isOp root && isLeaf l && isLeaf r             = (varlist, (BuildTree (count varlist (BuildTree root l r)) Leaf Leaf))
    | isOp root && not (isLeaf l) && not (isLeaf r) = countExp varlist (BuildTree root (snd (countExp varlist l)) (snd (countExp varlist r)))
    | isOp root && not (isLeaf l) && isLeaf r       = countExp varlist (BuildTree root (snd (countExp varlist l)) r)
    | isOp root && isLeaf l && not (isLeaf r)       = countExp varlist (BuildTree root l (snd (countExp varlist r)))

gcd' :: Int -> Int-> Int
gcd' a b
      | b == 0     = abs a
      | otherwise  = gcd' b (a `mod` b)

strgcd :: String -> String -> Int
strgcd num1 num2 = (gcd' (stringToNumber num1) (stringToNumber num2))

isderiv :: String -> String -> Bool
isderiv a b = gcd' (stringToNumber a) (stringToNumber b) == 1

makederivdTree :: Tree -> Int -> Tree
makederivdTree (BuildTree value left right) k = (BuildTree newvalue left right)
    where newvalue = show ((stringToNumber value) `div` (gcd' (stringToNumber value) k))

deriv3 :: (Tree, String) -> (Tree, String)
deriv3 ((BuildTree root l right), r)
    | (isOp root) && (isLeaf l) && (isLeaf right)           = ((BuildTree root newL newRight), newR1)
    | (isOp root) && (isLeaf l) == False && (isLeaf right)  = ((BuildTree root (fst (deriv3 (l,r))) newRight), newR)
    | (isOp root)                                           = ((BuildTree root (fst (deriv3 (l,r))) (fst (deriv3 (right,r)))), r) 
        where newL = (BuildTree (show ((stringToNumber (getRoot l)) `div` (strgcd (getRoot l)  r))) Leaf Leaf)
              newR = show ((stringToNumber r) `div` (strgcd (getRoot right) r))
              newR1 = show ((stringToNumber r) `div` (strgcd (show(strgcd (getRoot right) r))(getRoot l)))
              newRight = (BuildTree (show ((stringToNumber (getRoot right)) `div` (strgcd  (getRoot right) r))) Leaf Leaf)

        
deriv :: Tree -> Tree
deriv (BuildTree root Leaf Leaf) =  (BuildTree root Leaf Leaf) 
deriv (BuildTree root (BuildTree left Leaf Leaf) (BuildTree right Leaf Leaf)) --two numbers
    |root == "/" && isnumber left && (strgcd left right) == (stringToNumber right) = (BuildTree (show ((stringToNumber left) `div` (stringToNumber right))) Leaf Leaf)
    |root == "/" && isnumber left  && (strgcd left right) /= 1                     = (BuildTree "/" (BuildTree (show ((stringToNumber left) `div` (strgcd left right))) Leaf Leaf) (BuildTree (show ((stringToNumber right) `div` (strgcd left right)))  Leaf Leaf)) 
    |root /= "/"                                                                   = (BuildTree root (BuildTree left Leaf Leaf) (BuildTree right Leaf Leaf))

deriv (BuildTree root l r) 
    |root == "/" && ((isLeaf l) == False) && ((isLeaf r) == False)                 = (BuildTree root (fst (deriv3 (l, (getRoot (leftTree (brackets r)))))) (fst (deriv3 (r, (getRoot (leftTree (brackets l)))))))
    |root == "/" && ((isLeaf l) == False) && ((isLeaf r) == True)                  = (BuildTree root (fst(deriv3 (l,(getRoot r)))) (BuildTree (show newR) Leaf Leaf))
        where newR = (stringToNumber (getRoot r)) `div` (strgcd (getRoot(leftTree (brackets l))) (getRoot r))

deriv (BuildTree root (BuildTree left leftl leftr) (BuildTree right Leaf Leaf)) 
    |root == "/" && left == "*"  && isnumber right && (isderiv right (getRoot leftl)) && (isderiv right (getRoot leftr)) = (BuildTree root (deriv (BuildTree left leftl leftr))  (BuildTree right Leaf Leaf))
    |root == "/" && left == "*"  && isnumber right                                                                       = (BuildTree root (fst (deriv3 ((BuildTree left newll newlr), (show (strgcd right (getRoot (leftTree(brackets (BuildTree left leftl leftr))))))))) (BuildTree (show newR) Leaf Leaf))
    |otherwise = (BuildTree root (deriv (BuildTree left leftl leftr))  (BuildTree right Leaf Leaf))
        where newll = if (isnumber (getRoot leftl)) then makederivdTree leftl (stringToNumber right) else leftl
              newr = if (isnumber (getRoot leftl)) then (stringToNumber right) `div` (strgcd right (getRoot leftl)) else stringToNumber right
              newlr = if newr/=1  then makederivdTree leftr (stringToNumber right) else leftr
              newR =  (stringToNumber right) `div`(strgcd right (getRoot (leftTree (brackets (BuildTree left leftl leftr))))) 

deriv (BuildTree root (BuildTree left leftl leftr) (BuildTree right rightl rightr)) 
    |root == "/" && left == "*"  && isnumber right && (isderiv right (getRoot leftl)) && (isderiv right (getRoot leftr)) = (BuildTree root (deriv (BuildTree left leftl leftr))  (BuildTree right Leaf Leaf))
    |root == "/" && left == "*"  && isnumber right                                                         = deriv (BuildTree root (BuildTree left newll newlr) (BuildTree (show newR) rightl rightr))
    | otherwise = (BuildTree root (deriv (BuildTree left leftl leftr)) (deriv (BuildTree right rightl rightr)))
        where newll = makederivdTree leftl (stringToNumber right)
              newright = (stringToNumber right) `div` (strgcd right (getRoot leftl))
              newlr = if newright /=1  then makederivdTree leftr (stringToNumber right) else leftr
              newR = (stringToNumber right) `div` (strgcd right (getRoot (leftTree (brackets (BuildTree left leftl leftr))))) 
deriv (BuildTree root (BuildTree left Leaf Leaf) (BuildTree right rightl rightr)) = (BuildTree root  (BuildTree left Leaf Leaf) (deriv (BuildTree right rightl rightr)))

brackets :: Tree -> Tree
brackets (BuildTree a Leaf Leaf) = (BuildTree a Leaf Leaf)
brackets (BuildTree a b c)
    | (a == "+" || a == "-") && (isLeaf b) == False && (isLeaf c)   = if (strgcd (getRoot (leftTree (brackets b))) (getRoot c)) /= 1 then (BuildTree a (BuildTree (getRoot (brackets b)) (newleftel) (getRightTree (brackets b))) newC2) else (BuildTree a b c) -- (getRightTree (brackets b) da se razdeli na newleftelem
    | (a == "+" || a == "-")  && (isLeaf b) && (isLeaf c)           = (BuildTree "*" (BuildTree (show(strgcd (getRoot b) (getRoot c))) Leaf Leaf) (BuildTree a newB newC))
        where newB = (makederivdTree b (strgcd (getRoot b) (getRoot c)))
              newC = (makederivdTree c (strgcd (getRoot b) (getRoot c)))
              newleftel = (BuildTree (show (strgcd (getRoot (leftTree (brackets b))) (getRoot c))) Leaf Leaf)
              newC2 = (BuildTree (show ((stringToNumber (getRoot c)) `div` (strgcd (getRoot (leftTree (brackets b))) (getRoot c)))) Leaf Leaf)


mult :: (String, Tree) -> (String, Tree)
mult (chislo, (BuildTree root Leaf Leaf))  = (chislo, (BuildTree (show ((stringToNumber root) * (stringToNumber chislo))) Leaf Leaf))
mult (chislo, (BuildTree root left rigth)) = (chislo, (BuildTree (show ((stringToNumber root) * (stringToNumber chislo))) (snd (mult (chislo, left))) (snd (mult (chislo, rigth)))))

mnozh1 :: Tree -> String
mnozh1 (BuildTree a Leaf Leaf) = a
mnozh1 (BuildTree a b c)
    | a == "*" && (isLeaf b) && (isLeaf c)                  = show ((stringToNumber (getRoot b)) * (stringToNumber (getRoot c)))
    | a == "*" && (isLeaf b) == False && (isLeaf c)         = show ((stringToNumber (mnozh1 b)) * (stringToNumber (getRoot c)))
    | a == "*" && (isLeaf b) && (isLeaf c) == False         = show ((stringToNumber (mnozh1 c)) * (stringToNumber (getRoot b)))
    | a == "*" && (isLeaf b) == False && (isLeaf c) ==False = show ((stringToNumber (mnozh1 b)) * (stringToNumber (mnozh1 c)))
    | a == "+" && (isLeaf b) && (isLeaf c)                  = show ((stringToNumber (getRoot b)) + (stringToNumber (getRoot c)))
    | a == "+" && (isLeaf b) == False && (isLeaf c)         = show ((stringToNumber (mnozh1 b)) + (stringToNumber (getRoot c)))
    | a == "+" && (isLeaf b) && (isLeaf c) == False         = show ((stringToNumber (mnozh1 c)) + (stringToNumber (getRoot b)))
    | a == "+" && (isLeaf b) == False && (isLeaf c) ==False = show ((stringToNumber (mnozh1 b)) + (stringToNumber (mnozh1 c)))
    | a == "-" && (isLeaf b) && (isLeaf c)                  = show ((stringToNumber (getRoot b)) - (stringToNumber (getRoot c)))
    | a == "-" && (isLeaf b) == False && (isLeaf c)         = show ((stringToNumber (mnozh1 b)) - (stringToNumber (getRoot c)))
    | a == "-" && (isLeaf b) && (isLeaf c) == False         = show ((stringToNumber (mnozh1 c)) - (stringToNumber (getRoot b)))
    | a == "-" && (isLeaf b) == False && (isLeaf c) ==False = show ((stringToNumber (mnozh1 b)) - (stringToNumber (mnozh1 c)))

mnozh :: (Tree, Tree) -> String
mnozh (left, right) = show (strgcd (mnozh1 left) (mnozh1 right))

obshtZnamenatel :: [(String,String)] -> Tree -> ([(String,String)],Tree)
obshtZnamenatel currlist (BuildTree a Leaf Leaf) = (currlist,(BuildTree a Leaf Leaf))
obshtZnamenatel currlist (BuildTree root l r)
    | (root == "+" || root == "-") && (getRoot l) =="/" && (getRoot r) =="/"                         = (currlist,(BuildTree root newL newR))
    | (root == "+" || root == "-") && (getRoot l) =="/" && ((getRoot r) =="+" || (getRoot r) == "-") = (currlist, (BuildTree root newL (snd (obshtZnamenatel currlist r))))
    | (root == "+" || root == "-") && (getRoot r) =="/" && ((getRoot l) =="+" || (getRoot l) == "-") = (currlist,(BuildTree root (snd(obshtZnamenatel currlist l)) newR))
    | (root == "+" || root == "-") && (getRoot r) =="/" && (isLeaf l)                                = (currlist,(BuildTree root (BuildTree (show ( (stringToNumber (mnozh1 (getRightTree r))) * (stringToNumber(getRoot l)))) Leaf Leaf) r)) --v6mesto 6/3 e samo
    | (root == "+" || root == "-") && (getRoot l) =="/" && (isLeaf r)                                = (currlist,(BuildTree root l (BuildTree (show ( (stringToNumber (mnozh1 (getRightTree l))) * (stringToNumber(getRoot r)))) Leaf Leaf))) --v6mesto 6/3 e samo
        where mnozhitelL = show ((stringToNumber (getRoot (snd (countExp currlist (getRightTree r))))) `div` (stringToNumber (mnozh ((getRightTree r),(getRightTree l))))) 
              mnozhitelR = show ((stringToNumber (getRoot (snd (countExp currlist (getRightTree l))))) `div` (stringToNumber (mnozh ((getRightTree r),(getRightTree l)))))
              newRL = (snd (mult(mnozhitelR, (leftTree r))))
              newRR = (snd (mult(mnozhitelR, (getRightTree r))))
              newLL = (snd (mult(mnozhitelL, (leftTree l))))
              newLR = (snd (mult(mnozhitelL, (getRightTree l))))
              newL = (BuildTree "/" newLL newLR)
              newR = (BuildTree "/" newRL newRR)


copyTree :: Tree -> Tree
copyTree (BuildTree root Leaf Leaf) = (BuildTree root Leaf Leaf)
copyTree (BuildTree root l Leaf) = (BuildTree root (copyTree l) Leaf)
copyTree (BuildTree root Leaf r) = (BuildTree root Leaf (copyTree r))
copyTree (BuildTree root l r) = (BuildTree root (copyTree l) (copyTree r))

diferenc :: [(String,String)] -> Tree -> ([(String,String)],Tree)
diferenc l (BuildTree a b c)
    |(a == "+" || a == "-") && isLeaf b && isLeaf c && numberb && numberc                           = (l,Leaf)
    |(a == "+" || a == "-") && isLeaf b && (not (isLeaf c)) && numberb                              = (diferenc l c)
    |(a == "+" || a == "-") && isLeaf c && (not (isLeaf b)) && numberc                              = (diferenc l b)
    |(a == "+" || a == "-") && isLeaf b  && isLeaf c && alphab && alphac                            = (l,(BuildTree "2" Leaf Leaf))
    |(a == "+" || a == "-") && isLeaf b  && isLeaf c && ((alphab && numberc) || (alphac && numberb))= (l,(BuildTree "1" Leaf Leaf))
    |(a == "+" || a == "-") && not (isLeaf b ) && not (isLeaf c)  = (l,(BuildTree a (snd (diferenc l b)) (snd (diferenc l c))))
    |a == "*" && isLeaf b && isLeaf c && ((alphab && numberc) || (alphac && numberb)) = (l,(BuildTree newa Leaf Leaf))
    |a == "^" && isLeaf b && isLeaf c && numberc = (l,(BuildTree "*" newleft newright ))
            where alphab = searchLetter (getRoot b) l /= []
                  alphac = searchLetter (getRoot c) l /= []
                  numberb = isnumber (getRoot b)
                  numberc = isnumber (getRoot c)
                  newa = if numberb then getRoot b else getRoot c
                  newleft = c
                  helpvalue =show((stringToNumber (getRoot c)) -1)
                  newright = (BuildTree a b (BuildTree helpvalue Leaf Leaf ))

isVariable :: String -> Bool
isVariable  currexpr = isAlpha (head  currexpr) && (head (drop 1  currexpr)) == '='

isExpression :: String -> Bool
isExpression  currexpr = getSymbol  currexpr/=[] && not (isVariable  currexpr)

beginCommand :: String -> String -> [(String,String)] -> Tree
beginCommand com expr l 
    |com == [] = buildGrandTree expr
    |com == "build" = buildGrandTree expr
    |com == "countExp" = snd  (countExp l (buildGrandTree expr))
    |com == "deriv" = deriv (buildGrandTree expr)
    |com == "obshtZnam" = snd(obshtZnamenatel l (buildGrandTree expr))
    |com == "diferenc" = snd (diferenc l (buildGrandTree expr))
    |com == "brackets" = buildGrandTree expr
      

myRead :: [(String,String)] ->String -> String -> IO() 
myRead currlist exp comm = do  
    mystream <- getLine  
    if mystream == "End"
        then print(beginCommand comm exp currlist)  
        else do  
            if (isExpression mystream) then myRead currlist mystream comm
            else if (isVariable mystream) then myRead (([(head mystream)],[(last mystream)]):currlist) exp comm
            else myRead currlist exp mystream 


