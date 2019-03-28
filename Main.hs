-- Piotr Szulc 347277

import Data.Char
import Data.List
import Control.Applicative
import Control.Monad.State
import Control.Monad.Except
import System.Environment
import Text.Read (readMaybe)
import System.Exit (exitSuccess)

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

parseInput :: [String] -> [PostscriptCommand]
parseInput =  map matchStringToToken

matchStringToToken :: String -> PostscriptCommand
matchStringToToken ('+':'-':xs) = PSError $ "Invalid combination +- preceding " ++ xs
matchStringToToken ('+':'+':xs) = PSError $ "Invalid combination ++ preceding " ++ xs
matchStringToToken     ('+':xs) = matchStringToToken xs
matchStringToToken inputString = case inputString of
                "add" -> PSAdd
                "sub" -> PSSub
                "div" -> PSDiv
                "mul" -> PSMul
                "moveto" -> PSMoveto
                "lineto" -> PSLineto
                "rotate" -> PSClosepath
                "translate" -> PSTranslate
                _ -> case n' of
                        Just n ->      PSRationalNumber (toRational n)
                        Nothing ->     PSError inputString
                    where n' = readMaybe inputString :: Maybe Int
                    

data PSState = PSState { stack                     :: [R] 
                       , currentPoint              :: Maybe Point  
                       , startPointOfCurrentPath   :: Maybe Point
                       , currentTransformation     :: Transform
                       , picture                   :: Picture
                       }

initState =    PSState { stack                     = []
                       , currentPoint              = Nothing
                       , startPointOfCurrentPath   = Nothing
                       , currentTransformation     = m1
                       , picture                   = Picture []
                       }

type StateExcept e s a = ExceptT e (State s) a

evalPSCommands :: [PostscriptCommand] -> StateExcept PostscriptRuntimeError PSState Picture
evalPSCommands [] = gets picture
evalPSCommands (x:xs) = do
    evalPSCommand x
    evalPSCommands xs

-- Picture is stored in state and evalPSCommands is responsible for extracting it from state
-- hence type of results of following functions is StateExcept PostscriptRuntimeError PSState ()
-- and not StateExcept PostscriptRuntimeError PSState Picture

evalArithmeticOperationPS :: (R -> R -> R) -> StateExcept PostscriptRuntimeError PSState ()
evalArithmeticOperationPS op = do
    state <- get
    case stack state of
        r2:r1:xs -> put state{ stack = op r1 r2:xs }
        _        -> throwError PSErrorStack

evalPSCommand :: PostscriptCommand -> StateExcept PostscriptRuntimeError PSState ()
evalPSCommand (PSError token) = throwError $ PSUnknownToken token

evalPSCommand (PSRationalNumber r) = modify (\s -> s{stack = r:stack s})
evalPSCommand PSAdd = evalArithmeticOperationPS ((+) :: R -> R -> R)
evalPSCommand PSSub = evalArithmeticOperationPS ((-) :: R -> R -> R)
evalPSCommand PSMul = evalArithmeticOperationPS ((*) :: R -> R -> R)

evalPSCommand PSDiv = do
    state <- get
    case stack state of
        0:r1:xs   -> throwError PSErrorDivisonByZero
        r2:r1:xs  -> put state{ stack = (r1 / r2):xs }
        _         -> throwError PSErrorStack

evalPSCommand PSMoveto = do
    state <- get
    case stack state of 
        r2:r1:xs -> let newPoint = Point (r1, r2)
                        newTranslatedPoint = trpoint (currentTransformation state) newPoint
                            in put state{ currentPoint = Just newTranslatedPoint
                                        , startPointOfCurrentPath = Just newTranslatedPoint
                                        , stack = xs
                                        }
        _ -> throwError PSErrorStack

evalPSCommand PSLineto = do
    state <- get
    case currentPoint state of
        Nothing -> throwError PSErrorNoCurrentPoint
        Just p -> case stack state of
                        r2:r1:xs -> let newPoint = Point (r1, r2)
                                        newTranslatedPoint = trpoint (currentTransformation state) newPoint
                                        newLine = Line (p, newTranslatedPoint)
                                        newPicture = (&) (picture state) (Picture [newLine])
                                      in put state{ currentPoint = Just newTranslatedPoint
                                                  , stack = xs
                                                  , picture = newPicture
                                                  }
                        _ -> throwError PSErrorStack

evalPSCommand PSClosepath = do
    state <- get
    let startP = startPointOfCurrentPath state
    let curP = currentPoint state
    case (startP, curP) of
        (Nothing, _) -> return ()
        (_, Nothing) -> return ()
        (Just p1, Just p2) -> if p1 == p2 then return ()
                              else
                                    let newPicture = (&) (picture state) (Picture [Line (p2, p1)])
                                        in put state{ picture = newPicture
                                                    , currentPoint = startP
                                                    }

evalPSCommand PSRotate = do
    state <- get
    let previousTransformation = currentTransformation state
    case stack state of
        r:xs -> let newTransformation = previousTransformation >< (rotate r)
                    in put state{ currentTransformation = newTransformation
                                , stack = xs
                                }
        _ -> throwError PSErrorStack

evalPSCommand PSTranslate = do
    state <- get
    let previousTransformation = currentTransformation state
    case stack state of
        r2:r1:xs -> let newTransformation = previousTransformation >< (translate (Vec (r1, r2)))
                    in put state{ currentTransformation = newTransformation
                                , stack = xs
                                }
        _ -> throwError PSErrorStack

main :: IO ()
main = do
    scale <- getScaleFromArgsIO
    parsedInput <- parseInput . words <$> getContents

    either (\_ ->       putStr $ (prependProlog . appendEpilog) $ errorMessage)
           (\picture -> putStr $ (prependProlog . appendEpilog) . intRenderingToPSOutput $ renderScaled scale picture)
           (evalState (runExceptT (evalPSCommands parsedInput)) initState)


getScaleFromArgsIO :: IO Int
getScaleFromArgsIO = do
        args <- getArgs
        case args of
            (x:_) -> case n of
                        Just n -> return n
                        Nothing -> do
                            print $ "Not integer argument: " ++ x ++ ". Usage: ./Main [scale]"
                            exitSuccess
                    where n = readMaybe x :: Maybe Int
            _  -> return 1

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
errorMessage = "/Courier findfont 24 scalefont setfont 0 0 moveto (Error) show\n"