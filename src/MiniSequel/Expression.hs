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
    SequelBool Bool |
    SequelNumList [Double] |
    SquelStringList [String] |
    SequelFunctor SequelBuiltInFunc [SequelExpression] |
    SequelBoolOperation SequelBooleanOperator SequelExpression SequelExpression |
    SequelRelationalOperation SequelRelationalOperator SequelExpression SequelExpression |
    SequelStringOperation SequelStringOperator SequelExpression SequelExpression |
    SequelSymbolOperation SequelSymbolOperator SequelExpression SequelExpression |
    SequelNumericOperation SequelNumberOperator SequelExpression SequelExpression |
    SequelUnaryOperation String SequelExpression |
    SequelUsing [SequelExpression] |
    SequelOn SequelExpression |
    SequelNull |
    SequelDefault |
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
  (||.) = applyBooleanFunctor BooleanOr


  infixr 1 &&.    -- Boolean and
  (&&.) :: SequelExpression -> SequelExpression -> SequelExpression
  (&&.) = applyBooleanFunctor BooleanAnd

  infixr 2 |.     -- bitwise or
  (|.) :: SequelExpression -> SequelExpression -> SequelExpression
  (|.) = applyNumericFunctor BitOr

  infixr 2 ^.     -- bitwise xor
  (^.) :: SequelExpression -> SequelExpression -> SequelExpression
  (^.) = applyNumericFunctor BitXor

  infixr 3 &.     -- bitwsie and
  (&.) :: SequelExpression -> SequelExpression -> SequelExpression
  (&.) = applyNumericFunctor BitAnd

  infix 4 =.     -- comparison
  (=.) :: SequelExpression -> SequelExpression -> SequelExpression
  (=.) = applyRelationalFunctor Equal

  infix 4 <>.    -- different
  (<>.) :: SequelExpression -> SequelExpression -> SequelExpression
  (<>.) = applyRelationalFunctor Different

  infix 4 <.     -- lesser
  (<.) :: SequelExpression -> SequelExpression -> SequelExpression
  (<.) = applyRelationalFunctor Less


  infix 4 <=.    -- lesser or equal
  (<=.) :: SequelExpression -> SequelExpression -> SequelExpression
  (<=.) = applyRelationalFunctor LessEq


  infix 4 >.     -- greater
  (>.) :: SequelExpression -> SequelExpression -> SequelExpression
  (>.) = applyRelationalFunctor Greater


  infix 4 >=.    -- greater or equal
  (>=.) :: SequelExpression -> SequelExpression -> SequelExpression
  (>=.) = applyRelationalFunctor GreaterEq

  infixl 5 >>.    -- right shift
  (>>.) :: SequelExpression -> SequelExpression -> SequelExpression
  (>>.) = applyNumericFunctor BitRightShift

  infixl 5 <<. -- left shift
  (<<.) :: SequelExpression -> SequelExpression -> SequelExpression
  (<<.) = applyNumericFunctor BitLeftShift

  infixl 6 +.    -- addition
  (+.) :: SequelExpression -> SequelExpression -> SequelExpression
  (+.) = applyNumericFunctor Add

  infixl 6 -.    -- substracion
  (-.) :: SequelExpression -> SequelExpression -> SequelExpression
  (-.) = applyNumericFunctor Substract

  infixl 7 *.    -- multiplication
  (*.) :: SequelExpression -> SequelExpression -> SequelExpression
  (*.) = applyNumericFunctor Multiply


  infixl 7 /.    -- division
  (/.) :: SequelExpression -> SequelExpression -> SequelExpression
  (/.) = applyNumericFunctor Divide

  infixl 7 %.    -- modulus
  (%.) :: SequelExpression -> SequelExpression -> SequelExpression
  (%.) = applyNumericFunctor Modulus

  infixr 8 **.   -- exponentiation
  (**.) :: SequelExpression -> SequelExpression -> SequelExpression
  (**.) a@SequelNumericOperation{} b@SequelNumericOperation{} = SequelFunctor POWER [a,b]
  (**.) a@(SequelNumber _) b@SequelNumericOperation{} = SequelFunctor POWER [a,b]
  (**.) a@SequelNumericOperation{} b@(SequelNumber _) = SequelFunctor POWER [a,b]
  (**.) a b = error $ "Unknown operatrion for (**) with:\n" `mappend` show a `mappend` "\n" `mappend` show b

  infix 9 ~>    -- column accessor
  (~>) :: SequelExpression -> SequelExpression -> SequelExpression
  (~>) a@(SequelSymbol _) b@(SequelSymbol _) = SequelSymbolOperation Access a b
  (~>) a b = error $ "Unknown operatrion for (~>) with:\n"`mappend`show a `mappend`"\n"`mappend` show b

  infixl 6 =:=   -- string concatenation
  (=:=) :: SequelExpression -> SequelExpression -> SequelExpression
  (=:=) a b = SequelFunctor CONCATENATE [a,b]

  infix 0 =: -- AS operator
  (=:) :: SequelExpression -> SequelExpression -> SequelExpression
  (=:) a s@(SequelSymbol _) = SequelSymbolOperation As a s

  quoteIdentifier = '`'
  quoteString = '\''

  instance Show SequelExpression where
    show (SequelBool True) = "true"
    show (SequelBool False) = "false"
    show (SequelString s) = s
    show (SequelNumber n) = show n
    show (SequelIntegral i) = show i
    show (SequelSymbol s) = quoteIdentifier:s `mappend` [quoteIdentifier]
    show (SequelSymbolOperation s a b) = show a `mappend` show s `mappend` show b
    show (SequelBoolOperation s a b) = "("`mappend`show a `mappend` show s `mappend` show b`mappend`")"
    show (SequelStringOperation s a b) = "("`mappend`show a `mappend` show s `mappend` show b`mappend`")"
    show (SequelNumericOperation s a b) = "("`mappend`show a `mappend` show s `mappend` show b`mappend`")"
    show (SequelRelationalOperation s a b) = "("`mappend`show a `mappend` show s `mappend` show b`mappend`")"
    show SequelNull = "NULL"
    show SequelDefault = "DEFAULT"
    show CurrentTimeStamp = "CURRENT_TIMESTAMP"
    show (SequelOn exp) = "ON " `mappend` show exp
    show (SequelUsing fields) = "USING (" `mappend` fs `mappend`")"
      where
       fs = intercalate ", " $ fmap show fields
    show (SequelFunctor func params) =
      show func `mappend`
      "(" `mappend`
      intercalate ", " (fmap show params) `mappend`
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


  isValue (SequelNumber _) = True
  isValue (SequelIntegral _) = True
  isValue SequelNumericOperation{} = True
  isValue (SequelString _ ) = True
  isValue SequelStringOperation{} = True
  isValue (SequelSymbol _) = True
  isValue SequelSymbolOperation{} = True
  isValue (SequelFunctor _ _) = True
  isValue (SequelBool _) = True

  between f a b = f >=. a &&. f <=. b

  applyFunction construct op a b
    | isValue a && isValue b = construct op a b
    | otherwise = error $ "Unknown operation for " `mappend` show op `mappend` " with:\n" `mappend` show a `mappend` "\n" `mappend` show b



  applyNumericFunctor :: SequelNumberOperator -> SequelExpression -> SequelExpression  -> SequelExpression
  applyNumericFunctor = applyFunction SequelNumericOperation

  applyRelationalFunctor :: SequelRelationalOperator -> SequelExpression -> SequelExpression -> SequelExpression
  applyRelationalFunctor = applyFunction SequelRelationalOperation

  applyBooleanFunctor :: SequelBooleanOperator -> SequelExpression -> SequelExpression -> SequelExpression
  applyBooleanFunctor op a@SequelRelationalOperation{} b@SequelRelationalOperation{} = SequelBoolOperation op a b
  applyBooleanFunctor op a@SequelBoolOperation{} b@SequelRelationalOperation{} = SequelBoolOperation op a b
  applyBooleanFunctor op a@SequelRelationalOperation{} b@SequelBoolOperation{} = SequelBoolOperation op a b
  applyBooleanFunctor op a@SequelBoolOperation{} b@SequelBoolOperation{} = SequelBoolOperation op a b
  applyBooleanFunctor op a b = error $ "Unknown operation for "`mappend`show op`mappend`" with:\n"`mappend`show a `mappend`"\n"`mappend` show b
