{-# LANGUAGE DeriveDataTypeable #-}
module MiniSequel.Model
where
  import MiniSequel.Expression
  import Data.List (intercalate)
  import Data.Data
  import qualified Data.Map as Map

  class SequelModel a where
    create_model :: Model a

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
    _primary_key :: Bool,
    _auto_increment :: Bool,
    _unique :: Bool
  }

  data Model a = Model{
    _name' :: SequelExpression,
    _columns :: [SequelField],
    _safe_creation :: Bool
  }

  if_not_exists m = m { _safe_creation = True}

  table name cols = Model { _name' = name, _columns = cols, _safe_creation = False}

  column :: SequelExpression -> SequelType -> SequelField
  column name@(SequelSymbol _) type' = SequelField {
      _type = type',
      _name = name,
      _default = Nothing,
      _null = True,
      _primary_key = False,
      _auto_increment = False,
      _unique = False
    }

  not_null :: SequelField -> SequelField
  not_null field = field { _null = False }

  auto_increment :: SequelField -> SequelField
  auto_increment field = field { _auto_increment = True }

  primary_key :: SequelField -> SequelField
  primary_key field = field { _primary_key = True }

  default' :: SequelExpression -> SequelField -> SequelField
  default' value field = field { _default = Just value}

  unique ::SequelField -> SequelField
  unique field = field { _unique = True }

  show_null True = " NULL "
  show_null False = " NOT NULL "

  show_default Nothing = ""
  show_default (Just val) = " DEFAULT "++show val

  show_auto_increment False = ""
  show_auto_increment True = " AUTO_INCREMENT "

  show_primary_key False = ""
  show_primary_key True = " PRIMARY KEY "

  show_fields fields = intercalate ", " $ map show fields


  instance Show SequelType where
    show SequelInteger = "INTEGER"
    show (SequelVarchar size) = "VARCHAR(" ++ show size ++ ")"
    show SequelDate = "DATE"
    show SequelDateTime = "DATETIME"
    show SequelTime = "TIME"
    show SequelDouble = "DOUBLE"
    show SequelText = "TEXT"
    show SequelBoolean = " BOOLEAN"
    show (SequelEnumeration values)= "ENUM(" ++ show_fields values ++ ")"


  instance Show (Model a) where
    show (Model name@(SequelSymbol _) fields safe) =
      "CREATE TABLE " ++
      (if safe then " IF NOT EXISTS " else "") ++
      show name ++
      "(" ++
      show_fields fields ++
      ")"

  instance Show SequelField where
    show (SequelField t name def nul pk ai uni) =
      show name ++
      " " ++
      show t ++
      show_null nul ++
      show_default def ++
      show_auto_increment ai ++
      show_primary_key pk ++
      if uni then (" UNIQUE ") else ""
