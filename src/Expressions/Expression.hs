module MiniSequel.Expression where

  data SequelExpression = 
    SequelValue String |
    SequelNumber Double |
    SequelSymbol String | 
    SequelOperation String SequelExpression SequelExpression |
    SequelUnaryOperation String SequelExpression 

  infixr 0 ||.    -- Boolean or
  (||.) :: SequelExpression -> SequelExpression -> SequelExpression
  (||.) a b = SequelOperation " OR " a b


  infixr 1 &&.    -- Boolean and
  (&&.) :: SequelExpression -> SequelExpression -> SequelExpression
  (&&.) a b = SequelOperation " AND " a b


  infixr 2 |.     -- bitwise or
  (|.) :: SequelExpression -> SequelExpression -> SequelExpression
  (|.) a b = SequelOperation " | " a b


  infixr 2 ^.     -- bitwise xor
  (^.) :: SequelExpression -> SequelExpression -> SequelExpression
  (^.) a b = SequelOperation " XOR " a b


  infixr 3 &.     -- bitwsie and
  (&.) :: SequelExpression -> SequelExpression -> SequelExpression
  (&.) a b = SequelOperation " & " a b


  infix 4 =.     -- comparison
  (=.) :: SequelExpression -> SequelExpression -> SequelExpression
  (=.) a b = SequelOperation " = " a b


  infix 4 <>.    -- different
  (<>.) :: SequelExpression -> SequelExpression -> SequelExpression
  (<>.) a b = SequelOperation " <> " a b


  infix 4 <.     -- lesser
  (<.) :: SequelExpression -> SequelExpression -> SequelExpression
  (<.) a b = SequelOperation " < " a b


  infix 4 <=.    -- lesser or equal
  (<=.) :: SequelExpression -> SequelExpression -> SequelExpression
  (<=.) a b = SequelOperation " <= " a b


  infix 4 >.     -- greater 
  (>.) :: SequelExpression -> SequelExpression -> SequelExpression
  (>.) a b = SequelOperation " > " a b


  infix 4 >=.    -- greater or equal
  (>=.) :: SequelExpression -> SequelExpression -> SequelExpression
  (>=.) a b = SequelOperation " >= " a b


  infixl 5 >>.    -- right shift
  (>>.) :: SequelExpression -> SequelExpression -> SequelExpression
  (>>.) a b = SequelOperation " >> " a b


  infixl 5 <<.    -- left shift
  (<<.) :: SequelExpression -> SequelExpression -> SequelExpression
  (<<.) a b = SequelOperation " << " a b


  infixl 6 +.    -- addition
  (+.) :: SequelExpression -> SequelExpression -> SequelExpression
  (+.) a b = SequelOperation " + " a b


  infixl 6 -.    -- substracion
  (-.) :: SequelExpression -> SequelExpression -> SequelExpression
  (-.) a b = SequelOperation " - " a b


  infixl 7 *.    -- multiplication
  (*.) :: SequelExpression -> SequelExpression -> SequelExpression
  (*.) a b = SequelOperation " * " a b


  infixl 7 /.    -- division
  (/.) :: SequelExpression -> SequelExpression -> SequelExpression
  (/.) a b = SequelOperation " / " a b


  infixl 7 %.    -- modulus
  (%.) :: SequelExpression -> SequelExpression -> SequelExpression
  (%.) a b = SequelOperation " MOD " a b


  infixr 8 **.   -- exponentiation
  (**.) :: SequelExpression -> SequelExpression -> SequelExpression
  (**.) a b = SequelOperation "POW" a b


  infix 9 ~>    -- column accessor
  (~>) :: SequelExpression -> SequelExpression -> SequelExpression
  (~>) a@(SequelSymbol _) b@(SequelSymbol _) = SequelOperation "." a b



  infixl 6 =:=   -- string concatenation
  (=:=) :: SequelExpression -> SequelExpression -> SequelExpression
  (=:=) a b = SequelOperation "CONCAT" a b


  instance Show SequelExpression where
    show (SequelValue s) = '\'':(escape_sql s) ++ "'" 
    show (SequelNumber n) = show n
    show (SequelSymbol s) = '`':(escape_sql s) ++ "`"
    show (SequelOperation "." a@(SequelSymbol _) b@(SequelSymbol _)) = (show a) ++ "." ++ (show b) 
    show (SequelOperation "CONCAT" a b) = "CONCAT("++(show a)++","++(show b)++")"
    show (SequelOperation "POW" a b) = "POW("++(show a)++","++(show b)++")"
    show (SequelOperation s a b) = "(" ++ (show a) ++ ")" ++ (s) ++ "(" ++ (show b) ++")"
    show (SequelUnaryOperation s a) = (s) ++ "("++(show a)++ ")"


  escape_sql :: String -> String
  escape_sql x = x

  sym = SequelSymbol
  num = SequelNumber
  str = SequelValue  

--  sym "table" ~> sym "attemps"  >=. num 3 &&. sym "loan_id" =. str "123123" &&. sym "salario" *. num 3 <>. num 10000 
--  x >= 3 and loan_id = '123123' and salario*3 != 100000