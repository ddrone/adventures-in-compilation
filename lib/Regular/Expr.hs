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

star :: Re -> Re
star = Iterate

cat :: Re -> Re -> Re
cat = Concat

char :: Char -> Re
char = Char

optional :: Re -> Re
optional r = Union r Epsilon

oneOf :: String -> Re
oneOf s = case s of
  [] -> Empty
  c : cs -> Union (Char c) (oneOf cs)

range :: Char -> Char -> Re
range from to = oneOf [from..to]
