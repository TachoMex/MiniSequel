{-# LANGUAGE DataKinds
           , TypeSynonymInstances
           , FlexibleInstances
           , OverloadedStrings #-}
module MiniSequel
where
  import Data.List (intercalate)
  import Data.Maybe
  import qualified Data.Text as T
  import qualified Data.Text.Lazy as TL
  import qualified Data.ByteString.Char8 as BS
  import qualified Data.ByteString.Search as BS
  import Control.Monad.State

  import MiniSequel.Expression

  type Query = State SequelQuery ()

  data SequelQuery = SequelQuery {
    _queryType :: SequelQueryType,
    _colums :: Maybe [SequelExpression],
    _from :: SequelTable,
    _values :: Maybe [[SequelExpression]],
    _where :: Maybe SequelExpression,
    _order :: Maybe [SequelOrder],
    _group :: Maybe [SequelExpression],
    _having :: Maybe SequelExpression,
    _limit :: Maybe (Int, Int),
    _upsert :: SequelUpsertConflict
  } | PlainQuery String

  data SequelUpsertConflict = SequelUpsertKey SequelExpression |
                              SequelUpsertField SequelExpression |
                              SequelUpsertAuto |
                              SequelUpsertEmpty
  data SequelColumn = SequelColumn SequelExpression
  data SequelQueryType = INSERT | DELETE | UPDATE | SELECT deriving (Show, Eq)
  data SequelTable =
    SequelTable SequelExpression |
    SequelJoin SequelTable SequelTable SequelJoinType SequelExpression
  data SequelOrder = Asc SequelExpression | Desc SequelExpression
  data SequelJoinType = INNER | LEFT | RIGHT deriving (Show)


  showTable qi (SequelTable s) = showExpr qi ' ' s
  showTable qi (SequelJoin a b t expr) = "(" `mappend` showTable qi a `mappend` " "
                                             `mappend` show t `mappend` " JOIN "
                                             `mappend` showTable qi b `mappend`
                                             showExpr qi ' ' expr `mappend` ")"

  showOrd qi qs (Asc exp) = showExpr qi qs exp `mappend` " ASC"
  showOrd qi qs (Desc exp) = showExpr qi qs exp `mappend` " DESC"

  showCol qi qs (SequelColumn s@(SequelSymbol _)) = showExpr qi qs s


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
    _limit = Nothing,
    _upsert = SequelUpsertEmpty
  }

  select :: [SequelExpression] ->  Query
  select fields = modify $ \ query -> query {_queryType = SELECT, _colums = Just fields}

  where' :: SequelExpression -> Query
  where' cond = do
    query <- get
    put $ result query
    where
      result query
        | isNothing $ _where query = query {_where = Just cond}
        | otherwise = query {_where = Just (cond &&. fromJust (_where query))}

  empty = select [v (1 :: Int)]

  update :: [SequelExpression] -> Query
  update fields = modify $ \ query -> query {_queryType = UPDATE, _colums = Just fields}

  insert :: [SequelExpression] -> Query
  insert cols = modify $ \ query -> query {_queryType = INSERT, _colums = Just cols}

  values ::[[SequelExpression]] -> Query
  values vals = modify $ \ query -> query {_values = Just vals, _queryType =  INSERT }

  into = from

  delete :: Query
  delete = modify $ \ query -> query {_queryType = DELETE}

  orderBy :: [SequelOrder] -> Query
  orderBy criteria = modify $ \ query -> query {_order = Just criteria}

  groupBy :: [SequelExpression] -> Query
  groupBy criteria = modify $ \ query -> query {_group = Just criteria}

  having :: SequelExpression -> Query
  having cond = modify $ \ query -> query {_having = Just cond}

  applyJoin :: SequelJoinType -> SequelTable -> SequelExpression -> Query
  applyJoin type' table cond = do
      query <- get
      let tbl = _from query
      put $ query { _from = SequelJoin tbl table type' cond }


  join = innerJoin

  innerJoin :: SequelTable -> SequelExpression -> Query
  innerJoin = applyJoin INNER

  rightJoin :: SequelTable -> SequelExpression -> Query
  rightJoin = applyJoin RIGHT

  leftJoin :: SequelTable -> SequelExpression -> Query
  leftJoin = applyJoin LEFT


  on :: SequelExpression -> SequelExpression
  on = SequelOn

  using :: [SequelExpression] -> SequelExpression
  using = SequelUsing

  limit :: Int -> Query
  limit lim = modify limit'
    where
      limit' query
        | isNothing $ _limit query = query { _limit = Just (lim, 0) }
        | otherwise = query { _limit = Just (lim, offset') }
        where
          (_, offset') = fromJust $ _limit query

  offset :: Int -> Query
  offset off = modify func
    where
      func query
        | isNothing $ _limit query = query { _limit = Just (1, off )}
        | otherwise = query { _limit = Just (lim, off) }
          where (lim, _) = fromJust $ _limit query

  makeQuery t query = q
    where
      ((), q) = runState query (from t)

  first :: Query
  first = limit 1

  onDuplicateKeyUpdate query = query { _upsert = SequelUpsertAuto }
  onConflictKeyUpdate s query = query { _upsert = SequelUpsertKey s }

  showCols _ _ Nothing = "*"
  showCols qi qs (Just cols) = intercalate "," $ fmap (showExpr qi qs) cols

  showCond _ _ Nothing = ""
  showCond qi qs (Just cond) = " WHERE " `mappend` showExpr qi qs cond

  showOrder _ _ Nothing = ""
  showOrder qi qs (Just criteria) = " ORDER BY " `mappend` intercalate ", "  (fmap (showOrd qi qs) criteria)

  showGroup _ _ Nothing = ""
  showGroup qi qs (Just criteria) = " GROUP BY " `mappend` intercalate ", " (fmap (showExpr qi qs) criteria)

  showHaving _ _ Nothing = ""
  showHaving qi qs (Just cond) = " HAVING " `mappend` showExpr qi qs cond

  showSingleSet qi qs (SequelRelationalOperation Equal s@(SequelSymbol _) a) =
    showExpr qi qs s `mappend` " = " `mappend` showExpr qi qs a
  showSingleSet qi qs (SequelRelationalOperation Equal s@SequelSymbolOperation{} a) =
    showExpr qi qs s `mappend` " = " `mappend` showExpr qi qs a

  showSet qi qs (Just cols) = intercalate "," $ fmap (showSingleSet qi qs) cols

  showInsertCols _ _ Nothing = ""
  showInsertCols qi qs (Just cols) =
    " (" `mappend`
      intercalate ", " (fmap (showCol qi qs . SequelColumn) cols) `mappend`
    ")"

  showLimit Nothing = ""
  showLimit (Just (a,0)) = " LIMIT " `mappend` show a
  showLimit (Just (a,b)) = " LIMIT " `mappend` show a `mappend` " OFFSET " `mappend` show b

  showSimpleLimit Nothing = ""
  showSimpleLimit (Just (a, _)) = " LIMIT " `mappend` show a

  showVals qi qs (Just cols) =
    intercalate ", " $
      fmap (\ row ->
        "(" `mappend`
        intercalate ", " (fmap (showExpr qi qs) row) `mappend`
        ")"
      )
      cols

  showUpsert _ _  SequelUpsertEmpty _ _ = ""
  showUpsert qi qs (SequelUpsertKey k) (Just fields) (SequelTable (SequelSymbol tbname)) = "ON CONFLICT ON CONSTRAINT " `mappend`
                                                                                      showExpr qi qs k `mappend`
                                                                                      " DO UPDATE SET" `mappend` intercalate "," (fmap (showUpdateValue qi qs) fields)
  showUpsert qi qs (SequelUpsertField k) (Just fields) (SequelTable (SequelSymbol tbname)) = "ON CONFLICT (" `mappend`
                                                                                      showExpr qi qs k `mappend`
                                                                                      ") DO UPDATE SET" `mappend` intercalate ", " (fmap (showUpdateValue qi qs) fields)
  showUpsert qi qs SequelUpsertAuto (Just fields) _ = " ON DUPLICATE KEY UPDATE " `mappend`  intercalate ", " ( fmap ( \f -> showExpr qi qs f  `mappend` "=VALUES(" `mappend` showExpr qi qs f `mappend` ")" ) fields)

  showUpdateValue qi qs field = showExpr qi qs field `mappend` " = EXCLUDED." `mappend` showExpr qi qs field


  plainQuery = PlainQuery

  showQuery :: Char -> Char -> SequelQuery -> String
  showQuery qi qs (SequelQuery SELECT cols table _ cond order groupBy having lim _) =
    "SELECT " `mappend`
    showCols qi qs cols `mappend`
    " FROM " `mappend`
    showTable qi table `mappend`
    showCond qi qs cond `mappend`
    showOrder qi qs order `mappend`
    showGroup qi qs groupBy `mappend`
    showHaving qi qs having `mappend`
    showLimit lim

  showQuery qi qs (PlainQuery s) = s

  showQuery qi qs (SequelQuery UPDATE cols table _ cond _ _ _ lim _) =
    "UPDATE " `mappend`
    showTable qi table `mappend`
    " SET " `mappend`
    showSet qi qs cols `mappend`
    showCond qi qs cond `mappend`
    showSimpleLimit lim

  showQuery qi qs(SequelQuery INSERT cols table vals _ _ _ _ _ upsert) =
    "INSERT INTO " `mappend`
    showTable qi table `mappend`
    showInsertCols qi qs cols `mappend`
    " VALUES " `mappend`
    showVals qi qs vals `mappend`
    showUpsert qi qs upsert cols table


  showQuery qi qs (SequelQuery DELETE _ table _ cond _ _ _ lim _) =
    "DELETE FROM " `mappend`
    showTable qi table `mappend`
    showCond qi qs cond `mappend`
    showSimpleLimit lim


  t s@(SequelSymbol _) = SequelTable s
  t s@SequelSymbolOperation{} = SequelTable s
  ts = SequelTable . SequelSymbol
  s = SequelSymbol
  {-# ANN module ("HLint: ignore Use camelCase" :: String) #-}
  don'tEscape = SequelString
  f = SequelFunctor
  currentTimestamp = CurrentTimeStamp

  vdef = SequelDefault

  vi :: Int -> SequelExpression
  vi = v

  vf :: Double -> SequelExpression
  vf = v

  class SequelValue a where
    v :: a -> SequelExpression

  instance SequelValue T.Text where
     v s = SequelString $ T.unpack s

  instance SequelValue TL.Text where
     v s = SequelString $ TL.unpack s

  instance SequelValue BS.ByteString where
     v s = SequelString $ BS.unpack s

  instance SequelValue String where
    v = SequelString

  instance SequelValue Int where
    v = SequelIntegral

  instance SequelValue Double where
    v = SequelNumber

  instance SequelValue Bool where
    v = SequelBool
