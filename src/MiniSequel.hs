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
    _having :: Maybe SequelExpression,
    _limit :: Maybe (Int, Int)
  }

  data SequelColumn = SequelColumn SequelExpression
  data SequelQueryType = INSERT | DELETE | UPDATE | SELECT deriving (Show)
  data SequelTable =
    SequelTable (SequelExpression) |
    SequelJoin SequelTable SequelTable SequelJoinType SequelExpression
  data SequelOrder = Asc SequelExpression | Desc SequelExpression
  data SequelJoinType = INNER | LEFT | RIGHT deriving (Show)


  instance Show SequelTable where
    show (SequelTable s) = show s
    show (SequelJoin a b t expr) = "(" ++ show a ++ " " ++ show t ++ " JOIN " ++ show b ++ " ON " ++show expr ++ ")"

  instance Show SequelOrder where
    show (Asc exp) = show exp ++ " ASC"
    show (Desc exp) = show exp ++ " DESC"

  instance Show SequelColumn where
    show (SequelColumn s@(SequelSymbol _)) = show s


  from :: SequelTable -> SequelQuery
  from table = SequelQuery {
    _query_type = SELECT,
    _colums = Nothing,
    _from = table,
    _values = Nothing,
    _where = Nothing,
    _order = Nothing,
    _group = Nothing,
    _having = Nothing,
    _limit = Nothing
  }

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

  inner_join :: SequelTable -> SequelTable -> SequelExpression -> SequelTable
  inner_join a b cond = SequelJoin a b INNER cond

  right_join :: SequelTable -> SequelTable -> SequelExpression -> SequelTable
  right_join a b cond = SequelJoin a b RIGHT cond

  left_join :: SequelTable -> SequelTable -> SequelExpression -> SequelTable
  left_join a b cond = SequelJoin a b LEFT cond

  limit :: Int -> SequelQuery -> SequelQuery
  limit lim query
    | isNothing $ _limit query = query { _limit = Just (lim, 0) }
    | otherwise = query { _limit = Just (lim, offset') }
      where (_, offset') = fromJust $ _limit query

  offset :: Int -> SequelQuery -> SequelQuery
  offset offset' query
    | isNothing $ _limit query = query { _limit = Just (1, offset')}
    | otherwise = query { _limit = Just (lim, offset') }
      where (lim, _) = fromJust $ _limit query
  first :: SequelQuery -> SequelQuery
  first = limit 1

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

  show_limit Nothing = ""
  show_limit (Just (a,0)) = " LIMIT " ++ show a
  show_limit (Just (a,b)) = " LIMIT " ++ show a ++ " OFFSET " ++ show b

  show_simple_limit Nothing = ""
  show_simple_limit (Just (a, _)) = " LIMIT " ++ show a

  show_vals (Just cols) =
    intercalate ", " $
      map (\ row ->
        "(" ++
        intercalate ", " (map show row) ++
        ")"
      )
      cols

  show_query (SequelQuery SELECT cols table _ cond order group_by having lim) =
    "SELECT " ++
    show_cols cols ++
    " FROM " ++
    show table ++
    show_cond cond ++
    show_order order ++
    show_group group_by ++
    show_having having ++
    show_limit lim

  show_query (SequelQuery UPDATE cols table _ cond _ _ _ lim) =
    "UPDATE " ++
    show table ++
    " SET " ++
    show_set cols ++
    show_cond cond ++
    show_simple_limit lim

  show_query (SequelQuery INSERT cols table vals _ _ _ _ _) =
    "INSERT INTO " ++
    show table ++
    show_insert_cols cols ++
    " VALUES " ++
    show_vals vals

  show_query (SequelQuery DELETE _ table _ cond _ _ _ lim) =
    "DELETE FROM " ++
    show table ++
    show_cond cond ++
    show_simple_limit lim

  instance Show SequelQuery where
    show = show_query

  t s@(SequelSymbol _) = SequelTable s
  ts = SequelTable . SequelSymbol
  s = SequelSymbol
  n = SequelNumber
  v = SequelString
  f = SequelFunctor
  now = f NOW []
  current_timestamp = CurrentTimeStamp