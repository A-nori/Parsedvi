module Dump 
where

dump_list [] = return ()
dump_list (x:xs) = do
  putStrLn $ show x
  dump_list xs
