module Main where

import Sim

main = do let conf = Config 2000 0.2 0.1
          let base = [("Height", return 100), ("Weight", return 80)]
          let test = (\e -> 1)
          let gen0 = evolution conf base
          printGen $ take 5 $ run conf gen0 test

