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


byMeter :: (Int, Int, Int) -> Int -> Double
byMeter (num, den, mag) v =
  fromIntegral (v * mag * num) / (fromIntegral $ 10^10*den)

convertMeter2Point :: Double -> Double
convertMeter2Point v = v * 7227 / 2.54



---- dump Command
dumpCommand :: (Int, Int, Int) -> Command -> IO ()
dumpCommand _ (Set_Char c) = putStrLn $ "set_char " ++ [(toEnum $ fromEnum c)]
dumpCommand _ (Set _ c) = putStrLn $ "set " ++ (show c)
dumpCommand (num, den, mag) (Set_Rule a b) = putStrLn $ "set_rule " ++ (show $ convertMeter2Point $ byMeter (num, den, mag) a) ++ " " ++ (show $ convertMeter2Point $ byMeter (num, den, mag) b) ++ " pt"
dumpCommand _ (Put _ c) = putStrLn $ "put " ++ (show c)
dumpCommand (num, den, mag) (Put_Rule a b) = putStrLn $ "put_rule " ++ (show $ convertMeter2Point $ byMeter (num, den, mag) a) ++ " " ++ (show $ convertMeter2Point $ byMeter (num, den, mag) b) ++ " pt"
dumpCommand _ Nop = putStrLn "nop"
dumpCommand _ (Bop cs p) = putStrLn $ "bop" ++ (show cs) ++ " " ++ (show p)
dumpCommand _ Eop = putStrLn "eop"
dumpCommand _ Push = putStrLn "push"
dumpCommand _ Pop = putStrLn "pop"
dumpCommand (num, den, mag) (Command.Right _ b) = putStrLn $ "right : " ++ (show $ convertMeter2Point $ byMeter (num, den, mag) b) ++ " pt"
dumpCommand _ W0 = putStrLn "w0"
dumpCommand (num, den, mag) (W _ b) = putStrLn $ "w : " ++ (show $ convertMeter2Point $ byMeter (num, den, mag) b) ++ " pt"
dumpCommand _ X0 = putStrLn "x0"
dumpCommand (num, den, mag) (X _ b) = putStrLn $ "x : " ++ (show $ convertMeter2Point $ byMeter (num, den, mag) b) ++ " pt"
dumpCommand (num, den, mag) (Down _ a) = putStrLn $ "down : " ++ (show $ convertMeter2Point $ byMeter (num, den, mag) a) ++ " pt"
dumpCommand _ Y0 = putStrLn "y0"
dumpCommand (num, den, mag) (Y _ a) = putStrLn $ "y : " ++ (show $ convertMeter2Point $ byMeter (num, den, mag) a) ++ " pt"
dumpCommand _ Z0 = putStrLn "z0"
dumpCommand (num, den, mag) (Z _ a) = putStrLn $ "z : " ++ (show $ convertMeter2Point $ byMeter (num, den, mag) a) ++ " pt"
dumpCommand _ (Fnt_Num f) = putStrLn $ "fnt_num : " ++ (show f)
dumpCommand _ (Fnt _ k) = putStrLn $ "fnt : " ++ (show k)
dumpCommand _ (Xxx _ _ x) = putStrLn $ "xxx : " ++ x
dumpCommand _ (Fnt_Def _ k c s d a l n) = do
  putStrLn $ "fnt_def : " ++ n
dumpCommand _ (Pre i n d m _ x) = do
  putStrLn $ "dvi version : " ++ (show i)
  putStrLn $ "numerator : " ++ (show n)
  putStrLn $ "denominator : " ++ (show d)
  putStrLn $ "magnification : " ++ (show m)
  putStrLn $ "comment : " ++ x


-- dump preamble
dumpPre :: Command -> IO (Int, Int, Int)
dumpPre (pre@(Pre _ n d m _ _)) = do
  dumpCommand (n, d, m) pre
  return (n, d, m)



-------- dump main
dumpMain :: (Int, Int, Int) -> [Command] -> Int -> IO ()
dumpMain _ [] _ = return ()
-- Push
dumpMain (num, den, mag) (Push:coms) n = do
  putIndent n
  dumpCommand (num, den, mag) Push
  dumpMain (num, den, mag) coms (n + 1)
-- Pop
dumpMain (num, den, mag) (Pop:coms) n = do
  putIndent (n - 1)
  dumpCommand (num, den, mag) Pop
  dumpMain (num, den, mag) coms (n - 1)
-- otherwise
dumpMain (num, den, mag) (com:coms) n = do
  putIndent n
  dumpCommand (num, den, mag) com
  dumpMain (num, den, mag) coms n

-------- dump postamble
dumpPost :: [Command] -> IO ()
dumpPost [] = return ()
dumpPost ((post@(Post p n d m l u s t)):coms) = do
  putStrLn $ show post
  dumpPost coms 
dumpPost ((fdef@(Fnt_Def i k c s d a l n)):coms) = do
  putStrLn $ show fdef
  dumpPost coms
dumpPost ((post@(Post_Post q i)):coms) = do
  putStrLn $ show post
  dumpPost coms

------ dumpCommands 
dumpCommands :: [Command] -> IO ()
dumpCommands coms = do
  let 
    (pre, main, post) = separate coms
  putStrLn "----- preamble -----"
  (num, den, mag) <- dumpPre pre
  putChar '\n'
  putStrLn "----- main -----"
  dumpMain (num, den, mag) main 0
  putChar '\n'
  putStrLn "----- postamble -----"
  dumpPost post

  
