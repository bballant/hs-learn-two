{-# LANGUAGE ScopedTypeVariables #-}

module BbCsv where

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import qualified Data.Map as Map

fidelity = "2018-05-16-fidelity.csv"

dval :: String -> Double
dval x = read x

getKV row = let v = dval (last row)
                k = row !! 2
            in (k, v)
              

foo :: IO ()
foo = do
    csvData <- BL.readFile fidelity --"demo.csv"
    case decode HasHeader csvData of
        Left err -> putStrLn err
        Right v -> let kvs = V.map getKV v
                   in V.forM_ kvs $ \ (k', v') -> putStrLn $ k' ++ " " ++ show v'
        --Right v -> putStrLn $ (show . dval . last) ((V.head v) :: [String])
        --Right v -> V.forM_ v $ \ (xs :: [String]) -> putStrLn $ show (head xs)
