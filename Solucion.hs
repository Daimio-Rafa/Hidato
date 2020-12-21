import Tablero
import System.Random
import System.IO

module(

)where


Solution:: Int -> Int -> [Box] -> [Box]
Solution line column ((Box boxLine boxColumn boxValue):tail) = 
                                                | line*column == boxValue = (Box boxLine boxColumn boxValue)++tail
                                                | otherwise Solve
                                                let firstBox = searchBoxForValue 1 box

Solve:: Int -> Int -> [Box] -> [Box]
