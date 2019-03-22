import Data.Char
import Data.List
import Data.Function
import Text.Read
import Control.Applicative
import qualified Data.Map as Map

data PostscriptCommand = PSNumber Int
                       | PSAdd
                       | PSSub
                       | PSDiv
                       | PSMul
                       | PSMoveto
                       | PSLineto
                       | PSClosepath
                       | PSError
                       deriving (Show, Eq)

-- main = interact foldrLoop
main = interact (show . parseInput . splitByWords)

splitByWords = filter (not . isSpace . head) . groupBy ((==) `on` not . isSpace)

foldrLoop :: String -> String
foldrLoop input = foldr (\x y -> show (typeOfVariable x) ++ y) [] (splitByWords input)


typeOfVariable :: String -> PostscriptCommand
typeOfVariable s = case k of
        Just n -> PSNumber n
        _ -> PSError
    where k = readMaybe s :: Maybe Int

toPSNumber :: Maybe Int -> Maybe PostscriptCommand
toPSNumber (Just n) = Just (PSNumber n)
toPSNumber _ = Nothing

matchStringToToken :: String -> PostscriptCommand
matchStringToToken "moveto" = PSMoveto
matchStringToToken "lineto" = PSLineto
matchStringToToken "closepath" = PSClosepath
matchStringToToken "add" = PSAdd
matchStringToToken "sub" = PSSub
matchStringToToken "div" = PSDiv
matchStringToToken "mul" = PSMul
matchStringToToken n = case n' of
                Just n -> PSNumber n
                Nothing -> PSError
            where n' = readMaybe n :: Maybe Int

parseInput :: [String] -> [PostscriptCommand]
parseInput =  map (\s -> matchStringToToken s)

-- type PostscriptStack = List
-- executeCommand :: PostscriptCommand -> PostscriptStack -> Int -> Int -> 