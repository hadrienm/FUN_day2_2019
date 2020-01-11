import Data.Char
import System.IO
import System.Environment
import System.Exit
import Control.Applicative (liftA2)

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem a (x:xs)
    | a == x = True
    | otherwise = myElem a xs

safeDiv :: Int -> Int -> Maybe Int
safeDiv a b
    | b == 0 = Nothing
safeDiv a b = Just (a `div` b)

safeNth :: [a] -> Int -> Maybe a
safeNth [] x = Nothing
safeNth (x:y) b
    | b /= 0 = safeNth y (b - 1)
    | otherwise = Just x

safeSucc :: Maybe Int -> Maybe Int
safeSucc Nothing = Nothing
safeSucc (Just a) = Just (a + 1)

myLookup :: Eq a => a -> [(a, b)] -> Maybe b
myLookup a [] = Nothing
myLookup a ((x, xs):ys)
    | a == x = Just xs
    | otherwise = myLookup a ys

maybeDo :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
maybeDo a Nothing _ = Nothing
maybeDo a _ Nothing = Nothing
maybeDo a (Just b) (Just c) = Just (a b c)

isStringDigit :: [Char] -> Bool
isStringDigit [] = True
isStringDigit (x:xs)
    | x == '-' = isStringDigit(xs)
    | isDigit x == False = False
    | otherwise = isStringDigit(xs)

readInt :: [Char] -> Maybe Int
readInt [] = Nothing
readInt a
    | isStringDigit a == True = Just (read a :: Int)
    | otherwise = Nothing

getLineLength :: IO Int
getLineLength = do {
    name <- getLine
    ;return (length name)
}
printAndGetLength :: String -> IO Int
printAndGetLength a = do {
    putStrLn a
    ; return (length a)
}

printFirstLine :: Int -> Int -> IO ()
printFirstLine a b
    | a == 0 = return ()
printFirstLine a b = do {
    if (a * 2 == b * 2) then
        putStr("+")
    else if (a == 1) then
        putStrLn("+")
    else
        putStr("-")
    ;printFirstLine (a - 1) b
}

printBody :: Int -> Int -> Int -> IO ()
printBody a b c
    | c == 0 = return ()
printBody a b c
    | (a == 0 && c /= 0) = printBody b b (c - 1)

printBody a b c = do {
    if (a * 2 == b * 2) then
        putStr("|")
    else if (a == 1) then
        putStrLn("|")
    else
        putStr(" ")
    ;printBody (a - 1) b c
}

printBox :: Int -> IO ()
printBox a
    | a == 1 = putStrLn("++")
    | a <= 0 = return ()
printBox a = do {
    printFirstLine (a * 2) (a * 2)
    ;printBody (a * 2) (a * 2) (a - 2)
    ;printFirstLine (a * 2) (a * 2)   
}

(+++) :: Applicative f => f [a] -> f [a] -> f [a]
(+++) = liftA2 (++)

concatLines :: Int -> IO String
concatLines a
    | a <= 0 = return ("")
concatLines a =
    if a == 1 then
        getLine
    else do 
        getLine +++ concatLines (a - 1)

concatLine :: Int -> IO String
concatLine a
    | a <= 0 = return ("")
    | otherwise  = do
        name <- getLine
        name2 <- concatLine(a - 1)
        return (name ++ name2)

getInt :: IO (Maybe Int)
getInt = do {
    ;value <- getLine
    ;return (readInt value) 
}

main :: IO ()
main = do
    args <- getArgs
    if length (args) /= 3 then
        exitWith (ExitFailure 84)
    else if args!!1 == "/" && args!!2 == "0" then 
        exitWith (ExitFailure 84)
    else if (args!!1 /= "+" && args!!1 /= "%" && args!!1 /= "*" && args!!1 /= "-" && args!!1 /= "/") || isStringDigit (args!!0) == False || isStringDigit (args!!2) == False then
        exitWith (ExitFailure 84)
    else
        print $ doop (read (args!!0) :: Int) (args!!1) (read (args!!2) :: Int)

doop :: Int -> String -> Int -> Int
doop a b c
    | b == "/" = a `div` c
    | b == "*" = a * c
    | b == "+" = a + c
    | b == "-" = a - c
    | b == "%" = a `mod` c