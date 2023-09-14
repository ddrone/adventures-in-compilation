module Regular.Expr where

data Re
  = Char Char
  | Epsilon
  | Empty
  | Union Re Re
  | Concat Re Re
  | Iterate Re
  deriving (Show)

string :: String -> Re
string s = case s of
  [] -> Epsilon
  c : cs -> Concat (Char c) (string cs)

plus :: Re -> Re
plus r = Concat r (Iterate r)

optional :: Re -> Re
optional r = Union r Epsilon

range :: String -> Re
range s = case s of
  [] -> Empty
  c : cs -> Union (Char c) (range cs)

