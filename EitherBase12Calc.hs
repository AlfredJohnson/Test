-- Reverse Polish Base 12 Calculator!
-- Created By Andrew Fryer in 2017 with inpiration from Learn You a Haskell For Great Good
import Control.Monad
type Denary = Double  --Base 10 system
type Dozenal = [Char] --Base 12 System
type DenaryInt = Int
type DenaryDigit = Int
type DozenalInt = [Char]
type DozenalDigit = Char
main = do  
    putStrLn ("This is a reverse polish base twelve calculator!\n"
              ++ "Try typing something like \"1E.4 X2 / 3.4 ^ 101.4X8 - 16 +\"\n"
              ++ "Ten (10) in base 10 (the denary system) is dek (\"X\") in base 12 (the dozenal system), and eleven is el (\"E\")."
              ++ "Type \"quit\" to end the program") 
    loop 
loop = do
        expression <- getLine
        if expression /= "quit" then do
                                        putStrLn . fromEither . input $ expression
                                        loop
                                else return ()
    where
        input :: String -> Either String Denary
        input xs = do
                    ans <- (foldM f [] . words $ xs) >>= errorCheck
                    return ans
            where
                f :: [Denary] -> Dozenal -> Either String [Denary]
                f (x:y:ys) "+" = Right $ (x + y):ys
                f (x:y:ys) "-" = Right $ (y - x):ys  
                f (x:y:ys) "*" = Right $ (x * y):ys
                f (x:y:ys) "/" = Right $ (x / y):ys
                f (x:y:ys) "^" = Right $ (x ** y):ys
                f xs s = if s `elem` ["+","-","*","/","^"]                          --This checks if s is actually an operator
                            then Left "not enough operands for the given operators" --which means that there weren't enough operands for it
                            else (:xs) <$> dozToDen s                               --Normal value
                errorCheck :: [Denary] -> Either String Denary
                errorCheck ys = case length ys of 0 -> Left "not enough operands for the given operators" --This case only applies when input is called with ""
                                                  1 -> Right $ (\[y] -> y) ys
                                                  _ -> Left "not enough operators for the given operands"
        fromEither :: Either String Denary -> String
        fromEither (Right xs) = "Answer: " ++ denToDoz xs
        fromEither (Left xs) = "Error: " ++ xs
dozToDen :: Dozenal -> Either String Denary
dozToDen xs = addNegative <$> addDecimal <$> (intDozToDen $ toPositiveInt xs)
    where
        toPositiveInt = filter (/='.') . filter (/='-')
        intDozToDen :: DozenalInt -> Either String DenaryInt
        intDozToDen []     = Right 0 --remove if I can later
        intDozToDen (x:xs) = ((+) <$> intDozToDen xs) <*> ((* (12^length xs)) <$> charDozToDen x)
            where
                charDozToDen :: DozenalDigit -> Either String DenaryDigit
                charDozToDen 'E' = Right 11
                charDozToDen 'X' = Right 10
                charDozToDen x   = if isNumber then Right (read [x]) else Left "Not a Number (or an operator)"
                    where isNumber = x `elem` ['1' .. '9']
        figuresAfterDecimal []     = 0
        figuresAfterDecimal (x:xs) = if x == '.' then length xs else figuresAfterDecimal xs
        addDecimal = \x -> fromIntegral x / 12^figuresAfterDecimal xs
        addNegative = \y -> if head xs == '-' then  ((-1)*y) else  y
denToDoz :: Denary -> Dozenal
denToDoz x = addNegative $ addFirstZero $ stopInfiniteDecimals $ h (abs x) $ floor $ logBase 12 $ abs x
    where
        h :: Denary -> Int -> Dozenal
        h 0 n = if n >= 0 then take (n+1) $ repeat '0' else [] --n is the largest power of 12 needed to express x
        h x n = addDecimal $ charDenToDoz digit: h (x - fromIntegral digit * 12 ** fromIntegral n) (n - 1)
            where
                digit = floor $ x / 12 ** fromIntegral n
                charDenToDoz :: DenaryDigit -> DozenalDigit
                charDenToDoz 10 = 'X'
                charDenToDoz 11 = 'E'
                charDenToDoz x  = (\[x] -> x) $ show x --add error handling or replace with intToDigit x 
                addDecimal = if n == -1 then ('.':) else id
        stopInfiniteDecimals = \xs -> if '.' `elem` xs then (takeWhile (/='.') xs) ++ (cutOffDecimals $ dropWhile (/='.') xs) else xs --this prevents tiny remainders from creaing an infinite line of zeros
            where 
                cutOffDecimals []     = []
                cutOffDecimals (x:xs) = if take 10 (x:xs) /= (take 10 $ repeat '0') then x:cutOffDecimals xs else []
        addFirstZero = \xs -> if head xs == '.' then '0':xs else xs
        addNegative = \xs -> if (head $ show x) == '-' then '-':xs else xs