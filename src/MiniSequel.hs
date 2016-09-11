{-# LANGUAGE DataKinds #-}
module MiniSequel
where
  import Data.List
  import Data.Maybe
  import MiniSequel.Expression
  data SequelQuery = SequelQuery {
    _query_type :: SequelQueryType, 
    _colums :: Maybe [SequelExpression],
    _from :: SequelTable,
    _values :: Maybe [SequelExpression],
    _where :: Maybe SequelExpression,
    _order :: Maybe SequelOrder,
    _group :: Maybe [SequelColumn],
    _having :: Maybe SequelExpression
  }

  data SequelColumn = SequelColumn SequelExpression
  data SequelQueryType = INSERT | DELETE | UPDATE | SELECT deriving (Show)
  data SequelTable = SequelTable String | SequelJoin SequelTable SequelTable SequelJoinType SequelExpression
  data SequelOrder = Asc [SequelColumn] | Desc [SequelColumn]
  data SequelJoinType = INNER | LEFT | RIGHT deriving (Show)

  instance Show SequelTable where
    show (SequelTable s) = s
    show (SequelJoin a b t expr) = show a ++ show " " ++ show t ++ " JOIN " ++ show b ++ show expr

  instance Show SequelOrder where
    show (Asc arr) = "ASC " ++ (intercalate "," $ map show arr)

  instance Show SequelColumn where
    show (SequelColumn (SequelSymbol s)) = show s 
 

  from :: SequelExpression -> SequelQuery
  from table@(SequelSymbol _) = SequelQuery {
    _query_type = SELECT, 
    _colums = Nothing,
    _from = SequelTable $ show table, 
    _values = Nothing,
    _where = Nothing, 
    _order = Nothing, 
    _group = Nothing, 
    _having = Nothing
  }

  test = 
    update [users ~> nombre =. v"tacho", edad =. n 22] $ 
    where' (users ~> nombre =. v"pony" &&. edad >=. n 18) $ 
    from $ users
    where 
      users = s "users"
      nombre = s "nombre"
      edad = s "edad"
      id = s "id"


  select :: [SequelExpression] -> SequelQuery -> SequelQuery
  select fields query = 
    query {_query_type = SELECT, _colums = Just fields}

  where' :: SequelExpression -> SequelQuery -> SequelQuery
  where' cond query = 
    query {_where = (Just cond)}

  update :: [SequelExpression] -> SequelQuery -> SequelQuery
  update fields query = 
    query {_query_type = UPDATE, _colums = (Just fields)}

  show_cols Nothing = "*"
  show_cols (Just cols) = intercalate "," $ map show cols

  show_cond Nothing = ""
  show_cond (Just cond) = " WHERE " ++ show cond

  show_order Nothing = ""

  show_group Nothing = ""

  show_having Nothing = ""

  show_single_set (SequelRelationalOperation Equal s@(SequelSymbol _) a) = 
    show s ++ " = " ++ show a 
  show_single_set (SequelRelationalOperation Equal s@(SequelSymbolOperation _ _ _) a) =
    show s ++ " = " ++ show a 
  show_set (Just cols) = intercalate "," $ map show_single_set cols


  show_query (SequelQuery SELECT cols table _ cond order_by group_by having) = 
    "SELECT " ++ 
    show_cols cols ++
    " FROM " ++ 
    show table ++ 
    show_cond cond ++ 
    show_order order_by ++ 
    show_group group_by ++
    show_having having

  show_query (SequelQuery UPDATE cols table _ cond _ _ _) =
    "UPDATE " ++
    show table ++
    " SET " ++
    show_set cols ++
    show_cond cond 

  instance Show SequelQuery where
    show = show_query  


