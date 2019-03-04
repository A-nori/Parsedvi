module Command (
  Command
  ) where

import qualified Data.ByteString as B


data Command =
    Set_Char Int
  | Set Int Int
  | Set_Rule Int Int
  | Put Int Int
  | Put_Rule Int Int
  | Nop
  | Bop
  | Eop
  | Push
  | Pop
  | Right Int Int
  | W Int Int
  | X Int Int
  | Down Int Int
  | Y Int Int
  | Z Int Int
  | Fnt_Num Int Int
  | Fnt Int Int





convert [] = []
convert (x : xs) =
  | 0 <= x && x <= 127 = SetChar x
  | x <= 131 = Set x
