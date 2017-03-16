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

  showDefault _ _ Nothing = ""
  showDefault qi qs (Just val) = " DEFAULT "`mappend` showExpr qi qs val

  showAutoIncrement False = ""
  showAutoIncrement True = " AUTO_INCREMENT "

  showPrimaryKey False = ""
  showPrimaryKey True = " PRIMARY KEY "

  showFields qi qs table fields = intercalate ", " $ fmap (showField qi qs table) fields

  showType _ _ SequelInteger = "INTEGER"
  showType _ _ SequelFKSerial = "BIGINT(20) UNSIGNED"
  showType _ _ (SequelVarchar size) = "VARCHAR(" `mappend` show size `mappend` ")"
  showType _ _ SequelDate = "DATE"
  showType _ _ SequelDateTime = "DATETIME"
  showType _ _ SequelTimeStamp = "TIMESTAMP"
  showType _ _ SequelTime = "TIME"
  showType _ _ SequelDouble = "DOUBLE"
  showType _ _ SequelText = "TEXT"
  showType _ _ SequelSerial = "SERIAL"
  showType _ _ SequelBoolean = " BOOLEAN"
  showType qi qs (SequelEnumeration values)= "ENUM(" `mappend`  intercalate "," (fmap (showExpr qi qs) values) `mappend` ")"


  showModel qi qs (Model name@(SequelSymbol _) fields safe) =
      "CREATE TABLE " `mappend`
      (if safe then " IF NOT EXISTS " else "") `mappend`
      showExpr qi qs name `mappend`
      "(" `mappend`
      showFields qi qs name fields `mappend`
      ")"


  showField qi qs (SequelSymbol tableName) (SequelConstraint SequelCUniqKey fields) = "UNIQUE KEY (" `mappend` intercalate ", " (fmap (showExpr qi qs) fields) `mappend` ")"
  showField qi qs (SequelSymbol tableName) (SequelConstraint SequelCPrimaryKey fields) = "PRIMARY KEY (" `mappend` intercalate ", " (fmap (showExpr qi qs) fields) `mappend` ")"
  showField qi qs (SequelSymbol tableName) (SequelField t name@(SequelSymbol nme) def nul pk ai uni fk) =
    showExpr qi qs name `mappend`
    " " `mappend`
    showType qi qs t `mappend`
    showNull nul `mappend`
    showDefault qi qs def `mappend`
    showAutoIncrement ai `mappend`
    showPrimaryKey pk `mappend`
    if uni then " UNIQUE " else "" `mappend`
    case fk of
      Nothing -> ""
      Just (SequelSymbolOperation Access tabS@(SequelSymbol tab)  colS@(SequelSymbol col)) ->
        ", CONSTRAINT " `mappend` showExpr qi qs (SequelSymbol $ "fk_" `mappend` tableName `mappend` "_" `mappend` nme) `mappend`
          " FOREIGN KEY(" `mappend` showExpr qi qs name `mappend` ") " `mappend`
          " REFERENCES " `mappend` showExpr qi qs tabS `mappend` "(" `mappend` showExpr qi qs colS `mappend` ")"
