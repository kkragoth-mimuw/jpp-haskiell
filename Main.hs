-- Piotr Szulc 347277

import Data.Char
import Data.List
import Data.Function
import Control.Applicative
import Control.Monad.State
import Control.Monad.Except
import System.Environment
import Text.Read (readMaybe)

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
                            | PSNoCurrentPoint
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


evalArithmeticOperationPS :: (R -> R -> R) -> [PostscriptCommand] -> StateExcept String PSState Picture
evalArithmeticOperationPS op xs = do
    state <- get
    let s = stack state
    case s of
        r2:r1:xs' -> put state{stack=(op r1 r2):xs'}
        _ -> throwError "Not exhaustive stack"
    evalPS xs
                       
evalPS :: [PostscriptCommand] -> StateExcept String PSState Picture
evalPS [] = gets picture
evalPS (PSError token:_) = throwError $ "Unknown token: " ++ token


evalPS (PSAdd:xs) = evalArithmeticOperationPS ((+) :: R -> R -> R) xs
evalPS (PSSub:xs) = evalArithmeticOperationPS ((-) :: R -> R -> R) xs
evalPS (PSMul:xs) = evalArithmeticOperationPS ((*) :: R -> R -> R) xs
evalPS (PSDiv:xs) = do
    state <- get
    let s = stack state
    case s of
        0:r1:xs'  -> throwError "Division by zzero"
        r2:r1:xs' -> put state{stack=(r1 / r2):xs'}
        _ -> throwError "Not exhaustive stack"
    evalPS xs

evalPS (PSMoveto:xs) = do
    state <- get
    let s = stack state
    case s of 
        r2:r1:xs' -> put state{ currentPoint=Just (Point (r1, r2))
                             , stack=xs'
                             }
        _ -> throwError "Not exhaustive stack"
    evalPS xs

type StateExcept e s a = ExceptT e (State s) a

main = do
    scale <- getScaleFromArgs <$> getArgs
    parsedInput <- parseInput . words <$> getContents

    let initState = PSState { stack                   = []
                            , currentPoint            = Nothing
                            , startPointOfCurrentPath = Nothing
                            , currentTransformation   = Transform (Vec (0, 0)) 0
                            , picture                 = Picture []
                            }

    print parsedInput

    let picture = evalState (runExceptT (evalPS parsedInput)) initState

    print picture


getScaleFromArgs :: [String] -> Int
getScaleFromArgs (x:_) = case n of
                        Just n -> n
                        Nothing -> 1
                    where n = readMaybe x :: Maybe Int
getScaleFromArgs _ = 1

prependProlog :: String -> String
prependProlog s = "300 400 translate\n" ++ s

appendEpilog :: String -> String
appendEpilog s = s ++ "\nstroke showpage"

errorMessage :: String
errorMessage = "/Courier findfont 24 scalefont setfont 0 0 moveto (Error) show"