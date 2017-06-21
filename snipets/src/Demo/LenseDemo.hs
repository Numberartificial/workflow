{-# LANGUAGE TemplateHaskell #-}

module LenseDemo
       ()where

import Control.Lens

data Point = Point {
           _x :: Double,
           _y :: Double
           } deriving (Show)

data Mario = Mario { _location :: Point } deriving (Show)

makeLenses ''Point
makeLenses ''Mario

player1 = Mario (Point 0 0)

main = print (location.x +~ 10 $ player1)
