{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE RankNTypes          #-}
module Main where

import qualified Data.Massiv.Array as A
import Data.Massiv.Array ( Array
                         , S
                         , B
                         , Ix1(..)
                         , Ix2(..)
                         , Ix3(..)
                         , Sz1(..)
                         , Sz2(..)
                         , Sz3(..)
                         , pattern Sz1
                         , pattern Sz2
                         , pattern Sz3
                         )
import Control.Monad.Trans
import Control.Monad.State
import Control.Lens

import Futhark
import Futhark.Types
import qualified Futhark.Entries as E

import Show
import IO
import Data.Maybe
import Data.Int
import Control.Monad
import Foreign.C.Types

type Futhark3d t = Array S Ix3 t

data CountState =
     CountState
     { _counter :: Int
     }
makeLenses ''CountState

type CountMonad c m = StateT CountState (FutT c m)

runCountMonad :: (forall c . CountMonad c IO a) -> IO a
runCountMonad f = do
  (a,s) <- runFutTWith [{-Debug 1-}] $ runStateT f (CountState 0)
  putStrLn $ show (s ^. counter)
  return a

message :: String -> CountMonad c IO ()
message string = lift . lift $ putStrLn string

scaleImage :: (String, Int, Int)
            -> CountMonad c IO ()
scaleImage (path, h, w) =
  do  message "start"
      img <- readImage word8ToFloat ("image/"++path++".png")
      ( output   :: Futhark3d Float) <- lift $
              do futImg <- toFuthark img
                 futOutput <- E.bicubicInterpolationImage (fromIntegral h) (fromIntegral w) futImg
                 fromFuthark futOutput
      writeImage floatToWord8 ("output/"++ path ++ "_" ++ show h ++ "_" ++ show w++".png") output
      counter += 1

tests :: [(String, Int,Int)]
tests = [ ("e",   10,   10 )
        , ("e",   10,   10 )
        , ("e",  100,  100 )
        , ("e",  200,  200 )
        , ("e", 1024, 1024 )
        , ("peacock", 1024, 1024)
        , ("peacock", 8, 8)
        , ("blank", 1024, 1024)
        ]

main :: IO ()
main = runCountMonad $ mapM_ scaleImage tests
