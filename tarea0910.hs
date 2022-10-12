


---------------------ejercicio 1--------------------
promedio :: (Float,Float,Float) -> Float
promedio (x,y,z) = (x+y+z)/3

---------------------ejercicio 2--------------------
ultimoDigito :: Integer -> Bool
ultimoDigito n = n `rem` 10 == 3

---------------------ejercicio 3--------------------
has3Digits :: (Integral a)=>a->Bool
has3Digits x
 | x>99 && x<1000 = True
 | otherwise= False

---------------------ejercicio 4--------------------
neg :: Int -> Bool
neg n = n<0

---------------------ejercicio 5--------------------

sumaDigitosR :: Integer -> Integer
sumaDigitosR n
 |n<10 =n
 |n <=99 = n `rem` 10 + sumaDigitosR (n `div` 10)
 | otherwise = error "Hay mas de dos dígitos"

---------------------ejercicio 6--------------------
even2Digit:: Int->Bool
even2Digit x
 | x>9 && x<100 = even (x `mod` 10) && even((x `div` 10) `mod` 10)
 | x>99 = error "El numero no es de dos digitos "

---------------------ejercicio 7--------------------
isPrimeNumber :: Int -> Bool
isPrimeNumber n
 |n ==2 ||n==3 ||n ==5||n==7 ||n==11 ||n==13||n==17||n==19 = True
 |otherwise = False

---------------------ejercicio 8--------------------
isPrimeNumber2 :: Int->Bool
isPrimeNumber2 n
 |(n ==2 ||n==3 ||n ==5||n==7 ||n==11 ||n==13||n==17||n==19)&& even n = True
 |otherwise = False


---------------------ejercicio 9--------------------
multiplos :: (Integral a)=> a->a-> Bool
multiplos num mul
 |  mod num mul == 0 = True
 |otherwise = False


---------------------ejercicio 10-------------------
equal2digits :: (Integral a)=> a->Bool
equal2digits x
 |x<10 = False
 |x <=99 = div x 10 == mod x 10

---------------------ejercicio 11-------------------
higher :: (Integral a) => a -> a -> a -> a
higher x y z = max x (max y z)

---------------------ejercicio 12-------------------
isEvenSum2Number :: (Integral a) => a -> a -> Bool
isEvenSum2Number x y = even (x+y)

---------------------ejercicio 13-------------------
sum2Digit2Number :: (Integral a) => a -> a -> a
sum2Digit2Number x y
    |x > 9 && x <100 && y > 9 && y <100 = (div x 10 + mod x 10) + (div y 10 + mod y 10)
    |otherwise = error "Algún número no tiene 2 dígitos"

--------------------ejercicio 14-------------------
sum3Digits :: (Integral a) => a -> a
sum3Digits x
    |x > 99 && x <1000 = (div x 100 + div (mod x 100) 10 + mod (mod x 100) 10)
    |otherwise = error "El número no tiene 3 dígitos"

--------------------ejercicio 15-------------------
equal3Digits :: (Integral a) => a -> Bool
equal3Digits x
    |(x > 99 && x <1000) && (x1 == x2 || x1 == x3 || x2 == x3)= True
    |(x > 99 && x <1000) && (x1 /= x2 && x1 /= x3 && x2 /= x3)= False
    |otherwise = error "El número no tiene 3 dígitos"
    where
        x1 = div x 100
        x2 = div (mod x 100) 10
        x3 = mod (mod x 100) 10


--------------------ejercicio 16-------------------
positionHigher3Digits :: Integer -> String
positionHigher3Digits x
    |(x > 99 && x <1000) && ((max x1 x2 == x1) && (max x1 x3 == x1))= "El mayor se encuentra en la pos 1"
    |(x > 99 && x <1000) && ((max x1 x2 == x2) && (max x2 x3 == x2))= "El mayor se encuentra en la pos 2"
    |(x > 99 && x <1000) && ((max x1 x3 == x3) && (max x2 x3 == x3))= "El mayor se encuentra en la pos 3"
    |otherwise = error "El número no tiene 3 dígitos"
    where
        x1 = div x 100
        x2 = div (mod x 100) 10
        x3 = mod (mod x 100) 10

--------------------ejercicio 17-------------------

palindromo :: Eq a => [a] -> Bool
palindromo xs = xs == reverse xs

--------------------ejercicio 18-------------------

safeDivision :: Double -> Double -> Double
safeDivision _ 0 = error "No es posible dividir entre 0"
safeDivision x y = x/y

--------------------ejercicio 19-------------------
xor' :: Bool -> Bool -> Bool
xor' True True = False 
xor' True False = True
xor' False True = True
xor' False False = False

--------------------ejercicio 20-------------------
distancia :: (Double,Double) -> (Double,Double) -> Double 
distancia (x1,y1) (x2,y2) = sqrt((x1-x2)**2+(y1-y2)**2)
