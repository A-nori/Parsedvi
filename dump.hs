module Dump 
where

import Command

indent = "  "
putIndent 0 = return ()
putIndent n = do
  putStr indent
  putIndent (n - 1)


dump_list :: [Command] -> Int -> IO ()
dump_list [] _ = return ()
-- Push
dump_list (Push:xs) n = do
  putIndent n
  putStrLn $ show Push
  dump_list xs (n + 1)
-- Pop
dump_list (Pop:xs) n = do
  putIndent n
  putStrLn $ show Pop
  dump_list xs (n - 1)
-- Set_Char
dump_list ((Set_Char x):xs) n = do
  putIndent n
  putStrLn ("Set_Char " ++ [(toEnum $ fromEnum x)])
  dump_list xs n
-- otehrwise
dump_list (x:xs) n = do
  putIndent n
  putStrLn $ show x
  dump_list xs n

