module MiniSequel.Model
where
  import MiniSequel.Expression
  import MiniSequel
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
    SequelTimeStamp |
    SequelTime |
    SequelDouble |
    SequelEnumeration [SequelExpression] |
    SequelText |
    SequelSerial |
    SequelFKSerial

  data SequelConstraintKey = SequelCUniqKey | SequelCPrimaryKey | SequelCForeignKey

  data SequelField = SequelField {
    _type :: SequelType,
    _name :: SequelExpression,
    _default :: Maybe SequelExpression,
    _null :: Bool,
    _primaryKey :: Bool,
    _autoIncrement :: Bool,
    _unique :: Bool,
    _foreignKey :: Maybe SequelExpression
  } | SequelConstraint SequelConstraintKey [SequelExpression]

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
  autoIncrement field = field { _autoIncrement = True , _type = SequelSerial }

  foreignKey :: SequelExpression -> SequelField -> SequelField
  foreignKey refer field = field { _foreignKey = Just refer }

  primaryKey :: SequelField -> SequelField
  primaryKey field = field { _primaryKey = True }

  default' :: SequelExpression -> SequelField -> SequelField
  default' value field = field { _default = Just value}

  unique :: SequelField -> SequelField
  unique field = field { _unique = True }

  uniqueKey :: [SequelExpression] -> SequelField
  uniqueKey = SequelConstraint SequelCUniqKey

  cPrimaryKey :: [SequelExpression] -> SequelField
  cPrimaryKey = SequelConstraint SequelCPrimaryKey


  showNull True = " NULL "
  showNull False = " NOT NULL "

  showDefault Nothing = ""
  showDefault (Just val) = " DEFAULT "++show val

  showAutoIncrement False = ""
  showAutoIncrement True = " AUTO_INCREMENT "

  showPrimaryKey False = ""
  showPrimaryKey True = " PRIMARY KEY "

  showFields table fields = intercalate ", " $ map (showField table) fields


  instance Show SequelType where
    show SequelInteger = "INTEGER"
    show SequelFKSerial = "BIGINT(20) UNSIGNED"
    show (SequelVarchar size) = "VARCHAR(" ++ show size ++ ")"
    show SequelDate = "DATE"
    show SequelDateTime = "DATETIME"
    show SequelTimeStamp = "TIMESTAMP"
    show SequelTime = "TIME"
    show SequelDouble = "DOUBLE"
    show SequelText = "TEXT"
    show SequelSerial = "SERIAL"
    show SequelBoolean = " BOOLEAN"
    show (SequelEnumeration values)= "ENUM(" ++ ( intercalate "," $ map show values) ++ ")"


  instance Show (Model a) where
    show (Model name@(SequelSymbol _) fields safe) =
      "CREATE TABLE " ++
      (if safe then " IF NOT EXISTS " else "") ++
      show name ++
      "(" ++
      showFields name fields ++
      ")"


  showField (SequelSymbol tableName) (SequelConstraint SequelCUniqKey fields) = "UNIQUE KEY (" ++ intercalate ", " (map show fields) ++ ")"
  showField (SequelSymbol tableName) (SequelConstraint SequelCPrimaryKey fields) = "PRIMARY KEY (" ++ intercalate ", " (map show fields) ++ ")"
  showField (SequelSymbol tableName) (SequelField t name@(SequelSymbol nme) def nul pk ai uni fk) =
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
        ", CONSTRAINT " ++ show (SequelSymbol $ "fk_" ++ tableName ++ "_" ++ nme) ++
          " FOREIGN KEY(" ++ show name  ++ ") " ++
          " REFERENCES " ++ show tabS ++ "(" ++ show colS ++ ")"
