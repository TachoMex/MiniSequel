{-# LANGUAGE DataKinds #-}
module MiniSequel
where
  import Data.List (intercalate)
  import Data.Maybe
  import MiniSequel.Expression
  data SequelQuery = SequelQuery {
    _query_type :: SequelQueryType, 
    _colums :: Maybe [SequelExpression],
    _from :: SequelTable,
    _values :: Maybe [[SequelExpression]],
    _where :: Maybe SequelExpression,
    _order :: Maybe [SequelOrder],
    _group :: Maybe [SequelExpression],
    _having :: Maybe SequelExpression
  }

  data SequelColumn = SequelColumn SequelExpression
  data SequelQueryType = INSERT | DELETE | UPDATE | SELECT deriving (Show)
  data SequelTable = SequelTable String | SequelJoin SequelTable SequelTable SequelJoinType SequelExpression
  data SequelOrder = Asc SequelExpression | Desc SequelExpression
  data SequelJoinType = INNER | LEFT | RIGHT deriving (Show)

  instance Show SequelTable where
    show (SequelTable s) = s
    show (SequelJoin a b t expr) = show a ++ show " " ++ show t ++ " JOIN " ++ show b ++ show expr

  instance Show SequelOrder where
    show (Asc exp) = show exp ++ " ASC"  
    show (Desc exp) = show exp ++ " DESC"  

  instance Show SequelColumn where
    show (SequelColumn s@(SequelSymbol _)) = show s 
 

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

  test_select = 
    select [users ~> nombre, edad, edad *. n 3 =: s"3_edades"] $ 
    where' (users ~> nombre =. v"pony" &&. edad >=. n 18) $ 
    from $ users
    where 
      users = s "users"
      nombre = s "nombre"
      edad = s "edad"
      id = s "id"

  test_update = 
    update [users ~> nombre =. v"tacho", edad =. n 22] $ 
    where' (users ~> nombre =. v"pony" &&. edad >=. n 18) $ 
    from $ users
    where 
      users = s "users"
      nombre = s "nombre"
      edad = s "edad"
      id = s "id"

  test_order = 
    select [users ~> nombre, edad, edad *. n 3 =: s"3_edades"] $ 
    where' (users ~> nombre =. v"pony" &&. edad >=. n 18) $ 
    order_by [Asc (nombre), Desc (edad >=. n 18), Asc (f LENGTH [edad])] $
    from users
    where 
      users = s "users"
      nombre = s "nombre"
      edad = s "edad"
      id = s "id"

  test_insert = 
    insert [id, nombre, edad] $
    values [
      [SequelNull, v"tacho", n 22],
      [SequelNull, v"lily", n 21],
      [SequelNull, v"bigui", n 22]
      ] $
    into users
      where
        users = s "users"
        id = s "id"
        nombre = s "nombre"
        edad = s "edad"

  test_delete =
    delete $ 
    where' (s"nombre" =. v"bigui") $
    from (s "users")

  full_test = 
    select [patente, pedimento, seccionaduanera, fraccion, f MAX [preciounitario] =: preciounitario] $
    where' (f YEAR [fechapagoreal] >. v"2015-12-31") $
    group_by [fraccion] $
    order_by [Desc preciounitario] $
    having (preciounitario >. n 10000) $
    from _551 
      where
        _551 = s"_551"
        patente = s"patente"
        pedimento = s"pedimento"
        seccionaduanera = s"seccionaduanera"
        fraccion = s"fraccion"
        preciounitario = s "preciounitario"
        fechapagoreal = s "fechapagoreal"






  select :: [SequelExpression] -> SequelQuery -> SequelQuery
  select fields query = 
    query {_query_type = SELECT, _colums = Just fields}

  where' :: SequelExpression -> SequelQuery -> SequelQuery
  where' cond query = 
    query {_where = (Just cond)}

  update :: [SequelExpression] -> SequelQuery -> SequelQuery
  update fields query = 
    query {_query_type = UPDATE, _colums = (Just fields)}

  insert :: [SequelExpression] -> SequelQuery -> SequelQuery
  insert cols query = 
    query {_query_type = INSERT, _colums = (Just cols)}

  values ::[[SequelExpression]] -> SequelQuery -> SequelQuery
  values vals query = 
    query {_values = Just vals}

  into = from 

  delete :: SequelQuery -> SequelQuery 
  delete query = query {_query_type = DELETE}

  order_by :: [SequelOrder] -> SequelQuery -> SequelQuery
  order_by criteria query = 
    query {_order = Just criteria}

  group_by :: [SequelExpression] -> SequelQuery -> SequelQuery
  group_by criteria query = 
    query {_group = Just criteria}

  having :: SequelExpression -> SequelQuery -> SequelQuery
  having cond query = 
    query {_having = Just cond}

  show_cols Nothing = "*"
  show_cols (Just cols) = intercalate "," $ map show cols

  show_cond Nothing = ""
  show_cond (Just cond) = " WHERE " ++ show cond

  show_order Nothing = ""
  show_order (Just criteria) = " ORDER BY " ++ intercalate ", "  (fmap show criteria)

  show_group Nothing = ""
  show_group (Just criteria) = " GROUP BY " ++ intercalate ", " (fmap show criteria)

  show_having Nothing = ""
  show_having (Just cond) = " HAVING " ++ show cond

  show_single_set (SequelRelationalOperation Equal s@(SequelSymbol _) a) = 
    show s ++ " = " ++ show a 
  show_single_set (SequelRelationalOperation Equal s@(SequelSymbolOperation _ _ _) a) =
    show s ++ " = " ++ show a 
  show_set (Just cols) = intercalate "," $ map show_single_set cols

  show_insert_cols Nothing = ""
  show_insert_cols (Just cols) = 
    " (" ++ 
      intercalate ", " (map (show.SequelColumn) cols) ++
    ")"

  show_vals (Just cols) = 
    intercalate ", " $ 
      map (\ row -> 
        "(" ++ 
        intercalate ", " (map show row) ++ 
        ")"
      )
      cols

  show_query (SequelQuery SELECT cols table _ cond order group_by having) = 
    "SELECT " ++ 
    show_cols cols ++
    " FROM " ++ 
    show table ++ 
    show_cond cond ++ 
    show_order order ++ 
    show_group group_by ++
    show_having having

  show_query (SequelQuery UPDATE cols table _ cond _ _ _) =
    "UPDATE " ++
    show table ++
    " SET " ++
    show_set cols ++
    show_cond cond 

  show_query (SequelQuery INSERT cols table vals _ _ _ _) = 
    "INSERT INTO " ++
    show table ++ 
    show_insert_cols cols ++
    " VALUES " ++
    show_vals vals

  show_query (SequelQuery DELETE _ table _ cond _ _ _) = 
    "DELETE FROM " ++
    show table ++
    show_cond cond

  instance Show SequelQuery where
    show = show_query  


