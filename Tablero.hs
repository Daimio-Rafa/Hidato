module Tablero(
    Box(..),
    Board(..),
    searchBoxLine,
    searchBoxColumn,
    searchBoxValue,
    searchBoxForLineAndColumn,
    searchBoxForValue,
    isInRange,
    doesBoxExist,
    remove
)where

import System.Random

data Box = Box { line::Int, column::Int, value::Int}

data Board = Board { height::Int, width::Int, board::[Box]}

searchBoxLine (Box line column value) = line

searchBoxColumn (Box line column value) = column

SearchBoxValue (Box line column value) = value


searchBoxForLineAndColumn:: Int -> Int -> [Box] -> Box
searchBoxForLineAndColumn line column ((Box boxLine boxColumn boxValue):tail)
                                                | line == boxLine && column == boxColumn = Box boxLine boxColumn boxValue
                                                | otherwise = searchBox line column tail

searchBoxForValue:: Int -> [Box] -> Box
searchBoxForValue value ((Box boxLine boxColumn boxValue):tail)
                                                | boxValue == value = Box boxLine boxColumn boxValue
                                                | searchLineForValue value tail



isInRange:: Box -> Board -> Bool
isInRange (Box boxLine boxColumn boxValue) (Board height width board) = 
                                                if 0 <= boxLine && boxLine <= height && 0 <= boxColumn && boxColumn <= width
                                                then True else False


doesBoxExist:: Int -> Int -> [Box] -> Bool
doesBoxExist _ _ [] = False
doesBoxExist line column ((Box boxLine boxColumn boxValue):tail) 
                                                | line == boxLine && column == boxColumn = True
                                                | length tail == 0 = False
                                                | otherwise = doesBoxExist line column tail


doesBoxExistValue value ((Box boxLine boxColumn boxValue):tail) | value == boxValue = True
               | length tail == 0 = False
               | otherwise = doesBoxExistValue value tail


remove:: Box -> [Box] -> [Box]
remove boxToRemove listOfBox = [result | result <- listOfBox, result /= boxToRemove]
