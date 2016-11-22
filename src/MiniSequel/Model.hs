module MiniSequel.Model
where
  import MiniSequel.Expression
  import Data.List (intercalate)
  import Data.Data
  import qualified Data.Map as Map

  class SequelModel a where
    createModel :: Model a

  data SequelType =
    SequelBoolean |
    SequelInteger |
    SequelVarchar Int |
    SequelDate |
    SequelDateTime |
    SequelTime |
    SequelDouble |
    SequelEnumeration [String] |
    SequelText

  data SequelField = SequelField {
    _type :: SequelType,
    _name :: SequelExpression,
    _default :: Maybe SequelExpression,
    _null :: Bool,
    _primaryKey :: Bool,
    _autoIncrement :: Bool,
    _unique :: Bool,
    _foreignKey :: Maybe SequelExpression
  } | SequelUniqueKey [SequelExpression]

  data Model a = Model{
    _name' :: SequelExpression,
    _columns :: [SequelField],
    _safeCreation :: Bool
  }

  ifNotExists m = m { _safeCreation = True}

  table name cols = Model { _name' = name, _columns = cols, _safeCreation = False}

  column :: SequelExpression -> SequelType -> SequelField
  column name@(SequelSymbol _) type' = SequelField {
      _type = type',
      _name = name,
      _default = Nothing,
      _null = True,
      _primaryKey = False,
      _autoIncrement = False,
      _unique = False,
      _foreignKey = Nothing
    }

  notNull :: SequelField -> SequelField
  notNull field = field { _null = False }

  autoIncrement :: SequelField -> SequelField
  autoIncrement field = field { _autoIncrement = True }

  foreignKey :: SequelExpression -> SequelField -> SequelField
  foreignKey refer field = field {_foreignKey = Just refer }

  primaryKey :: SequelField -> SequelField
  primaryKey field = field { _primaryKey = True }

  default' :: SequelExpression -> SequelField -> SequelField
  default' value field = field { _default = Just value}

  unique :: SequelField -> SequelField
  unique field = field { _unique = True }

  uniqueKey :: [SequelExpression] -> SequelField
  uniqueKey = SequelUniqueKey

  showNull True = " NULL "
  showNull False = " NOT NULL "

  showDefault Nothing = ""
  showDefault (Just val) = " DEFAULT "++show val

  showAutoIncrement False = ""
  showAutoIncrement True = " AUTO_INCREMENT "

  showPrimaryKey False = ""
  showPrimaryKey True = " PRIMARY KEY "

  showFields fields = intercalate ", " $ map show fields


  instance Show SequelType where
    show SequelInteger = "INTEGER"
    show (SequelVarchar size) = "VARCHAR(" ++ show size ++ ")"
    show SequelDate = "DATE"
    show SequelDateTime = "DATETIME"
    show SequelTime = "TIME"
    show SequelDouble = "DOUBLE"
    show SequelText = "TEXT"
    show SequelBoolean = " BOOLEAN"
    show (SequelEnumeration values)= "ENUM(" ++ showFields values ++ ")"


  instance Show (Model a) where
    show (Model name@(SequelSymbol _) fields safe) =
      "CREATE TABLE " ++
      (if safe then " IF NOT EXISTS " else "") ++
      show name ++
      "(" ++
      showFields fields ++
      ")"

  instance Show SequelField where
    show (SequelUniqueKey fields) = "UNIQUE (" ++ intercalate ", " (map show fields) ++ ")"
    show (SequelField t name def nul pk ai uni fk) =
      show name ++
      " " ++
      show t ++
      showNull nul ++
      showDefault def ++
      showAutoIncrement ai ++
      showPrimaryKey pk ++
      if uni then " UNIQUE " else "" ++
      case fk of
        Nothing -> ""
        Just (SequelSymbolOperation Access tabS@(SequelSymbol tab)  colS@(SequelSymbol col)) ->
          ", CONSTRAINT " ++ show (SequelSymbol $ "fk" ++ tab) ++
            "FOREIGN KEY(" ++ show name  ++ ") " ++
            "REFERENCES " ++ show tabS ++ "(" ++ show colS ++ ")"
