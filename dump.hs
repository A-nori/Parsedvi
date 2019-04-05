module Dump 
where

import Command

indent = "  "
putIndent 0 = return ()
putIndent n = do
  putStr indent
  putIndent (n - 1)


-- get preamble
getPreamble :: [Command] -> (Command, [Command])
getPreamble coms@(Pre _ _ _ _ _ _:_) = (head coms, tail coms)

-- get postamble
-- tail recursion
getPostamble :: [Command] -> ([Command], [Command])
getPostamble coms = getPostamble_sub [] coms

getPostamble_sub :: [Command] -> [Command] -> ([Command], [Command])
getPostamble_sub acc post@(Post _ _ _ _ _ _ _ _:_) = (reverse acc, post)
getPostamble_sub acc (com:coms) = getPostamble_sub (com : acc) coms

-- [Command] to (preamble, main, postamble)
separate :: [Command] -> (Command, [Command], [Command])
separate coms =
  let 
    (pre, rest) = getPreamble coms
  in
  let 
    (main, post) = getPostamble rest
  in
    (pre, main, post)


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
