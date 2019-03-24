-- Piotr Szulc 347277

import Data.Char
import Data.List
import Control.Applicative
import Control.Monad.State
import Control.Monad.Except
import System.Environment
import Text.Read (readMaybe)

import Mon
import Lib

data PostscriptCommand = PSRationalNumber Rational
                       | PSAdd
                       | PSSub
                       | PSDiv
                       | PSMul
                       | PSMoveto
                       | PSLineto
                       | PSTranslate
                       | PSRotate
                       | PSClosepath
                       | PSError String
                       deriving (Show, Eq)

data PostscriptRuntimeError = PSErrorDivisonByZero
                            | PSErrorStack
                            | PSErrorNoCurrentPoint
                            | PSUnknownToken String
                            deriving Show

matchStringToToken :: String -> PostscriptCommand
matchStringToToken "moveto"     = PSMoveto
matchStringToToken "lineto"     = PSLineto
matchStringToToken "closepath"  = PSClosepath
matchStringToToken "add"        = PSAdd
matchStringToToken "sub"        = PSSub
matchStringToToken "div"        = PSDiv
matchStringToToken "mul"        = PSMul
matchStringToToken "translate"  = PSTranslate
matchStringToToken "rotate"     = PSRotate
matchStringToToken n = case n' of
                   Just n ->      PSRationalNumber (toRational n)
                   Nothing ->     PSError n
        where n' = readMaybe n :: Maybe Int

parseInput :: [String] -> [PostscriptCommand]
parseInput =  map matchStringToToken

data PSState = PSState { stack                     :: [R] 
                       , currentPoint              :: Maybe Point  
                       , startPointOfCurrentPath   :: Maybe Point
                       , currentTransformation     :: Transform
                       , picture                   :: Picture
                       }


evalArithmeticOperationPS :: (R -> R -> R) -> [PostscriptCommand] -> StateExcept PostscriptRuntimeError PSState Picture
evalArithmeticOperationPS op xs = do
    state <- get
    case stack state of
        r2:r1:xs' -> put state{stack=(op r1 r2):xs'}
        _ -> throwError PSErrorStack
    evalPS xs
                       
evalPS :: [PostscriptCommand] -> StateExcept PostscriptRuntimeError PSState Picture
evalPS [] = gets picture
evalPS (PSError token:_) = throwError $ PSUnknownToken token
evalPS (PSRationalNumber r:xs) = do
    state <- get
    put state{stack = r:stack state}
    evalPS xs
evalPS (PSAdd:xs) = evalArithmeticOperationPS ((+) :: R -> R -> R) xs
evalPS (PSSub:xs) = evalArithmeticOperationPS ((-) :: R -> R -> R) xs
evalPS (PSMul:xs) = evalArithmeticOperationPS ((*) :: R -> R -> R) xs
evalPS (PSDiv:xs) = do
    state <- get
    case stack state of
        0:r1:xs'  -> throwError PSErrorDivisonByZero
        r2:r1:xs' -> put state{stack=(r1 / r2):xs'}
        _ -> throwError PSErrorStack
    evalPS xs
evalPS (PSMoveto:xs) = do
    state <- get
    case stack state of 
        r2:r1:xs' -> let newPoint = Just (Point (r1, r2)) in
                            put state{ currentPoint=newPoint
                                     , startPointOfCurrentPath=newPoint
                                     , stack=xs'
                                     }
        _ -> throwError PSErrorStack
    evalPS xs
evalPS (PSLineto:xs) = do
    state <- get
    case currentPoint state of
        Nothing -> throwError PSErrorNoCurrentPoint
        Just p -> case stack state of
                        r2:r1:xs' -> let newPoint = Point (r1, r2)
                                         newTranslatedPoint = trpoint (currentTransformation state) newPoint
                                         newLine = Line (p, newTranslatedPoint)
                                         newPicture = (&) (picture state) (Picture [newLine])
                                in put state{ currentPoint=Just newPoint
                                         , stack=xs'
                                         , picture=newPicture
                                        }
                        _ -> throwError PSErrorStack
    evalPS xs

evalPS (PSClosepath:xs) = do
    state <- get
    let startP = startPointOfCurrentPath state
    let curP = currentPoint state

    case (startP, curP) of
        (Nothing, _) -> return ()
        (_, Nothing) -> return ()
        (Just p1, Just p2) -> if (p1 == p2) then
                                    return ()
                              else
                                    let newPicture = (&) (picture state) (Picture [Line (p2, p1)])
                                        in put state{picture=newPicture
                                                    ,currentPoint=startP
                                                    }
    evalPS xs

evalPS (PSRotate:xs) = do
    state <- get

    let previousTransformation = currentTransformation state

    case stack state of
        r:xs' -> let newTransformation = (><) previousTransformation (rotate r)
                    in put state{currentTransformation=newTransformation, stack=xs'}
        _ -> throwError PSErrorStack
        
    evalPS xs

evalPS (PSTranslate:xs) = do
    state <- get
    let previousTransformation = currentTransformation state
    case stack state of
        r2:r1:xs' -> let newTransformation = (><) previousTransformation (translate (Vec (r1, r2)))
                    in put state{currentTransformation=newTransformation, stack=xs'}
        _ -> throwError PSErrorStack
    evalPS xs

type StateExcept e s a = ExceptT e (State s) a

main = do
    scale <- getScaleFromArgs <$> getArgs
    parsedInput <- parseInput . words <$> getContents

    let initState = PSState { stack                   = []
                            , currentPoint            = Nothing
                            , startPointOfCurrentPath = Nothing
                            , currentTransformation   = m1
                            , picture                 = Picture []
                            }

    let eitherPicture = evalState (runExceptT (evalPS parsedInput)) initState

    case eitherPicture of
        Right picture -> putStr $ prependProlog . appendEpilog . intRenderingToPSOutput $ renderScaled scale picture
        Left _ -> putStr $ (prependProlog . appendEpilog) errorMessage


getScaleFromArgs :: [String] -> Int
getScaleFromArgs (x:_) = case n of
                        Just n -> n
                        Nothing -> 1
                    where n = readMaybe x :: Maybe Int
getScaleFromArgs _ = 1

lineToSimplePS :: IntLine -> String
lineToSimplePS ((a, b), (c, d)) = show a ++ " " ++ show b ++ " moveto " ++ show c ++ " " ++ show d ++ " lineto"
intRenderingToPSOutput :: IntRendering -> String
intRenderingToPSOutput [] = ""
intRenderingToPSOutput intRendering = (intercalate "\n" (map lineToSimplePS intRendering)) ++ "\n"

prependProlog :: String -> String
prependProlog s = "300 400 translate\n" ++ s

appendEpilog :: String -> String
appendEpilog s = s ++ "stroke showpage\n"

errorMessage :: String
errorMessage = "/Courier findfont 24 scalefont setfont 0 0 moveto (Error) show"