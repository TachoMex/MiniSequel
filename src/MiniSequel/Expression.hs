module MiniSequel.Expression where
  import Data.List (intercalate)

  data SequelBooleanOperator =
    BooleanOr |
    BooleanAnd

  data SequelRelationalOperator =
    Less |
    LessEq |
    Greater |
    GreaterEq |
    Equal |
    Different

  data SequelNumberOperator =
    Add |
    Substract |
    Multiply |
    Divide |
    Modulus |
    BitOr |
    BitAnd |
    BitXor |
    BitLeftShift |
    BitRightShift

  data SequelStringOperator = Concat
  data SequelSymbolOperator = Access | As

  data SequelExpression =
    SequelSymbol String |
    SequelString String |
    SequelNumber Double |
    SequelIntegral Int |
    SequelNumList [Double] |
    SquelStringList [String] |
    SequelFunctor SequelBuiltInFunc [SequelExpression] |
    SequelBoolOperation SequelBooleanOperator SequelExpression SequelExpression |
    SequelRelationalOperation SequelRelationalOperator SequelExpression SequelExpression |
    SequelStringOperation SequelStringOperator SequelExpression SequelExpression |
    SequelSymbolOperation SequelSymbolOperator SequelExpression SequelExpression |
    SequelNumericOperation SequelNumberOperator SequelExpression SequelExpression |
    SequelUnaryOperation String SequelExpression |
    SequelNull |
    CurrentTimeStamp

  data SequelBuiltInFunc =
    CONCATENATE |
    MAX |
    MIN |
    AVG |
    SUM |
    POWER |
    DISTINCT |
    IF |
    LENGTH |
    YEAR |
    MONTH |
    NOW deriving (Show)

  infixr 0 ||.    -- Boolean or
  (||.) :: SequelExpression -> SequelExpression -> SequelExpression
  (||.) = appy_boolean_functor BooleanOr


  infixr 1 &&.    -- Boolean and
  (&&.) :: SequelExpression -> SequelExpression -> SequelExpression
  (&&.) = appy_boolean_functor BooleanAnd

  infixr 2 |.     -- bitwise or
  (|.) :: SequelExpression -> SequelExpression -> SequelExpression
  (|.) = apply_numeric_functor BitOr

  infixr 2 ^.     -- bitwise xor
  (^.) :: SequelExpression -> SequelExpression -> SequelExpression
  (^.) = apply_numeric_functor BitXor

  infixr 3 &.     -- bitwsie and
  (&.) :: SequelExpression -> SequelExpression -> SequelExpression
  (&.) = apply_numeric_functor BitAnd

  infix 4 =.     -- comparison
  (=.) :: SequelExpression -> SequelExpression -> SequelExpression
  (=.) = apply_relational_functor Equal

  infix 4 <>.    -- different
  (<>.) :: SequelExpression -> SequelExpression -> SequelExpression
  (<>.) = apply_relational_functor Different

  infix 4 <.     -- lesser
  (<.) :: SequelExpression -> SequelExpression -> SequelExpression
  (<.) = apply_relational_functor Less


  infix 4 <=.    -- lesser or equal
  (<=.) :: SequelExpression -> SequelExpression -> SequelExpression
  (<=.) = apply_relational_functor LessEq


  infix 4 >.     -- greater
  (>.) :: SequelExpression -> SequelExpression -> SequelExpression
  (>.) = apply_relational_functor Greater


  infix 4 >=.    -- greater or equal
  (>=.) :: SequelExpression -> SequelExpression -> SequelExpression
  (>=.) = apply_relational_functor GreaterEq

  infixl 5 >>.    -- right shift
  (>>.) :: SequelExpression -> SequelExpression -> SequelExpression
  (>>.) = apply_numeric_functor BitRightShift

  infixl 5 <<. -- left shift
  (<<.) :: SequelExpression -> SequelExpression -> SequelExpression
  (<<.) = apply_numeric_functor BitLeftShift

  infixl 6 +.    -- addition
  (+.) :: SequelExpression -> SequelExpression -> SequelExpression
  (+.) = apply_numeric_functor Add

  infixl 6 -.    -- substracion
  (-.) :: SequelExpression -> SequelExpression -> SequelExpression
  (-.) = apply_numeric_functor Substract

  infixl 7 *.    -- multiplication
  (*.) :: SequelExpression -> SequelExpression -> SequelExpression
  (*.) = apply_numeric_functor Multiply


  infixl 7 /.    -- division
  (/.) :: SequelExpression -> SequelExpression -> SequelExpression
  (/.) = apply_numeric_functor Divide

  infixl 7 %.    -- modulus
  (%.) :: SequelExpression -> SequelExpression -> SequelExpression
  (%.) = apply_numeric_functor Modulus

  infixr 8 **.   -- exponentiation
  (**.) :: SequelExpression -> SequelExpression -> SequelExpression
  (**.) a@(SequelNumericOperation _ _ _) b@(SequelNumericOperation _ _ _) = SequelFunctor POWER [a,b]
  (**.) a@(SequelNumber _) b@(SequelNumericOperation _ _ _) = SequelFunctor POWER [a,b]
  (**.) a@(SequelNumericOperation _ _ _) b@(SequelNumber _) = SequelFunctor POWER [a,b]
  (**.) a b = error $ "Unknown operatrion for (**) with:\n"++show a ++"\n"++ show b



  infix 9 ~>    -- column accessor
  (~>) :: SequelExpression -> SequelExpression -> SequelExpression
  (~>) a@(SequelSymbol _) b@(SequelSymbol _) = SequelSymbolOperation Access a b
  (~>) a b = error $ "Unknown operatrion for (~>) with:\n"++show a ++"\n"++ show b



  infixl 6 =:=   -- string concatenation
  (=:=) :: SequelExpression -> SequelExpression -> SequelExpression
  (=:=) a b = SequelFunctor CONCATENATE [a,b]

  infix 0 =: -- AS operator
  (=:) :: SequelExpression -> SequelExpression -> SequelExpression
  (=:) a s@(SequelSymbol _) = SequelSymbolOperation As a s

  instance Show SequelExpression where
    show (SequelString s) = s
    show (SequelNumber n) = show n
    show (SequelIntegral i) = show i
    show (SequelSymbol s) = '`':s ++ "`"
    show (SequelSymbolOperation s a b) = show a ++ show s ++ show b
    show (SequelBoolOperation s a b) = "("++show a ++ show s ++ show b++")"
    show (SequelStringOperation s a b) = "("++show a ++ show s ++ show b++")"
    show (SequelNumericOperation s a b) = "("++show a ++ show s ++ show b++")"
    show (SequelRelationalOperation s a b) = "("++show a ++ show s ++ show b++")"
    show SequelNull = "NULL"
    show CurrentTimeStamp = "CURRENT_TIMESTAMP"
    show (SequelFunctor func params) =
      show func ++
      "(" ++
      intercalate ", " (map show params) ++
      ")"

  instance Show SequelNumberOperator where
    show Add = " + "
    show Substract = " - "
    show Multiply = " * "
    show Divide = " / "
    show Modulus = " % "
    show BitOr = " | "
    show BitAnd = " & "
    show BitXor = " ^ "
    show BitLeftShift = " << "
    show BitRightShift = " >> "

  instance Show SequelBooleanOperator where
    show BooleanOr = " OR "
    show BooleanAnd = " AND "

  instance Show SequelRelationalOperator where
    show Less = " < "
    show LessEq = " <= "
    show Greater = " > "
    show GreaterEq = " >= "
    show Equal = " = "
    show Different = " <> "

  instance Show SequelSymbolOperator where
    show Access = "."
    show As = " AS "

  instance Show SequelStringOperator where
    show Concat = "CONCAT"


  is_value (SequelNumber _) = True
  is_value (SequelIntegral _) = True
  is_value (SequelNumericOperation _ _ _) = True
  is_value (SequelString _ ) = True
  is_value (SequelStringOperation _ _ _) = True
  is_value (SequelSymbol _) = True
  is_value (SequelSymbolOperation _ _ _) = True
  is_value (SequelFunctor _ _) = True

  apply_function construct op a b
    | is_value a && is_value b = construct op a b
    | otherwise = error $ "Unknown operation for "++show op++" with:\n"++show a ++"\n"++ show b



  apply_numeric_functor :: SequelNumberOperator -> SequelExpression -> SequelExpression  -> SequelExpression
  apply_numeric_functor = apply_function SequelNumericOperation

  apply_relational_functor :: SequelRelationalOperator -> SequelExpression -> SequelExpression -> SequelExpression
  apply_relational_functor = apply_function SequelRelationalOperation

  appy_boolean_functor :: SequelBooleanOperator -> SequelExpression -> SequelExpression -> SequelExpression
  appy_boolean_functor op a@(SequelRelationalOperation _ _ _) b@(SequelRelationalOperation _ _ _) = SequelBoolOperation op a b
  appy_boolean_functor op a@(SequelBoolOperation _ _ _) b@(SequelRelationalOperation _ _ _) = SequelBoolOperation op a b
  appy_boolean_functor op a@(SequelRelationalOperation _ _ _) b@(SequelBoolOperation _ _ _) = SequelBoolOperation op a b
  appy_boolean_functor op a@(SequelBoolOperation _ _ _) b@(SequelBoolOperation _ _ _) = SequelBoolOperation op a b
  appy_boolean_functor op a b = error $ "Unknown operation for "++show op++" with:\n"++show a ++"\n"++ show b





--  s"table" ~> s"attemps"  >=. n 3 &&. s "loan_id" =. v "123123" &&. s "salario" *. n 3 <>. n 10000
--  x >= 3 and loan_id = '123123' and salario*3 != 100000
