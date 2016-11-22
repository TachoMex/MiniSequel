{-# LANGUAGE DataKinds #-}
module MiniSequel
where
  import Data.List (intercalate)
  import Data.Maybe
  import MiniSequel.Expression
  data SequelQuery = SequelQuery {
    _queryType :: SequelQueryType,
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
  data SequelQueryType = INSERT | DELETE | UPDATE | SELECT deriving (Show, Eq)
  data SequelTable =
    SequelTable SequelExpression |
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
    _queryType = SELECT,
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
    query {_queryType = SELECT, _colums = Just fields}

  where' :: SequelExpression -> SequelQuery -> SequelQuery
  where' cond query
    | isNothing $ _where query = query {_where = Just cond}
    | otherwise = query {_where = Just (cond &&. fromJust (_where query))}

  update :: [SequelExpression] -> SequelQuery -> SequelQuery
  update fields query =
    query {_queryType = UPDATE, _colums = Just fields}

  insert :: [SequelExpression] -> SequelQuery -> SequelQuery
  insert cols query =
    query {_queryType = INSERT, _colums = Just cols}

  values ::[[SequelExpression]] -> SequelQuery -> SequelQuery
  values vals query =
    query {_values = Just vals}

  into = from

  delete :: SequelQuery -> SequelQuery
  delete query = query {_queryType = DELETE}

  orderBy :: [SequelOrder] -> SequelQuery -> SequelQuery
  orderBy criteria query =
    query {_order = Just criteria}

  groupBy :: [SequelExpression] -> SequelQuery -> SequelQuery
  groupBy criteria query =
    query {_group = Just criteria}

  having :: SequelExpression -> SequelQuery -> SequelQuery
  having cond query =
    query {_having = Just cond}

  innerJoin :: SequelTable -> SequelTable -> SequelExpression -> SequelTable
  innerJoin a b = SequelJoin a b INNER

  rightJoin :: SequelTable -> SequelTable -> SequelExpression -> SequelTable
  rightJoin a b = SequelJoin a b RIGHT

  leftJoin :: SequelTable -> SequelTable -> SequelExpression -> SequelTable
  leftJoin a b = SequelJoin a b LEFT

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

  showCols Nothing = "*"
  showCols (Just cols) = intercalate "," $ map show cols

  showCond Nothing = ""
  showCond (Just cond) = " WHERE " ++ show cond

  showOrder Nothing = ""
  showOrder (Just criteria) = " ORDER BY " ++ intercalate ", "  (fmap show criteria)

  showGroup Nothing = ""
  showGroup (Just criteria) = " GROUP BY " ++ intercalate ", " (fmap show criteria)

  showHaving Nothing = ""
  showHaving (Just cond) = " HAVING " ++ show cond

  showSingleSet (SequelRelationalOperation Equal s@(SequelSymbol _) a) =
    show s ++ " = " ++ show a
  showSingleSet (SequelRelationalOperation Equal s@SequelSymbolOperation{} a) =
    show s ++ " = " ++ show a
  showSet (Just cols) = intercalate "," $ map showSingleSet cols

  showInsertCols Nothing = ""
  showInsertCols (Just cols) =
    " (" ++
      intercalate ", " (map (show.SequelColumn) cols) ++
    ")"

  showLimit Nothing = ""
  showLimit (Just (a,0)) = " LIMIT " ++ show a
  showLimit (Just (a,b)) = " LIMIT " ++ show a ++ " OFFSET " ++ show b

  showSimpleLimit Nothing = ""
  showSimpleLimit (Just (a, _)) = " LIMIT " ++ show a

  showVals (Just cols) =
    intercalate ", " $
      map (\ row ->
        "(" ++
        intercalate ", " (map show row) ++
        ")"
      )
      cols

  showQuery (SequelQuery SELECT cols table _ cond order groupBy having lim) =
    "SELECT " ++
    showCols cols ++
    " FROM " ++
    show table ++
    showCond cond ++
    showOrder order ++
    showGroup groupBy ++
    showHaving having ++
    showLimit lim

  showQuery (SequelQuery UPDATE cols table _ cond _ _ _ lim) =
    "UPDATE " ++
    show table ++
    " SET " ++
    showSet cols ++
    showCond cond ++
    showSimpleLimit lim

  showQuery (SequelQuery INSERT cols table vals _ _ _ _ _) =
    "INSERT INTO " ++
    show table ++
    showInsertCols cols ++
    " VALUES " ++
    showVals vals

  showQuery (SequelQuery DELETE _ table _ cond _ _ _ lim) =
    "DELETE FROM " ++
    show table ++
    showCond cond ++
    showSimpleLimit lim

  instance Show SequelQuery where
    show = showQuery

  t s@(SequelSymbol _) = SequelTable s
  ts = SequelTable . SequelSymbol
  s = SequelSymbol
  nd = SequelNumber
  ni = SequelIntegral
  b = SequelBool
  v s = SequelString $ show s
  don'tEscape = SequelString
  f = SequelFunctor
  now = f NOW []
  currentTimestamp = CurrentTimeStamp
