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


  instance Show SequelTable where
    show (SequelTable s) = show s
    show (SequelJoin a b t expr) = "(" `mappend` show a `mappend` " " `mappend` show t `mappend` " JOIN " `mappend` show b `mappend` show expr `mappend` ")"

  instance Show SequelOrder where
    show (Asc exp) = show exp `mappend` " ASC"
    show (Desc exp) = show exp `mappend` " DESC"

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
    _limit = Nothing,
    _upsert = SequelUpsertEmpty
  }

  select :: [SequelExpression] -> SequelQuery -> SequelQuery
  select fields query =
    query {_queryType = SELECT, _colums = Just fields}

  where' :: SequelExpression -> SequelQuery -> SequelQuery
  where' cond query
    | isNothing $ _where query = query {_where = Just cond}
    | otherwise = query {_where = Just (cond &&. fromJust (_where query))}

  empty = select [v (1 :: Int)]

  update :: [SequelExpression] -> SequelQuery -> SequelQuery
  update fields query =
    query {_queryType = UPDATE, _colums = Just fields}

  insert :: [SequelExpression] -> SequelQuery -> SequelQuery
  insert cols query =
    query {_queryType = INSERT, _colums = Just cols}

  values ::[[SequelExpression]] -> SequelQuery -> SequelQuery
  values vals query =
    query {_values = Just vals, _queryType =  INSERT }

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

  join :: SequelTable -> SequelTable -> SequelExpression -> SequelTable
  join = innerJoin

  innerJoin :: SequelTable -> SequelTable -> SequelExpression -> SequelTable
  innerJoin a b = SequelJoin a b INNER

  rightJoin :: SequelTable -> SequelTable -> SequelExpression -> SequelTable
  rightJoin a b = SequelJoin a b RIGHT

  leftJoin :: SequelTable -> SequelTable -> SequelExpression -> SequelTable
  leftJoin a b = SequelJoin a b LEFT

  on :: SequelExpression -> SequelExpression
  on = SequelOn

  using :: [SequelExpression] -> SequelExpression
  using = SequelUsing

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

  onDuplicateKeyUpdate query = query { _upsert = SequelUpsertAuto }
  onConflictKeyUpdate s query = query { _upsert = SequelUpsertKey s }

  showCols Nothing = "*"
  showCols (Just cols) = intercalate "," $ fmap show cols

  showCond Nothing = ""
  showCond (Just cond) = " WHERE " `mappend` show cond

  showOrder Nothing = ""
  showOrder (Just criteria) = " ORDER BY " `mappend` intercalate ", "  (fmap show criteria)

  showGroup Nothing = ""
  showGroup (Just criteria) = " GROUP BY " `mappend` intercalate ", " (fmap show criteria)

  showHaving Nothing = ""
  showHaving (Just cond) = " HAVING " `mappend` show cond

  showSingleSet (SequelRelationalOperation Equal s@(SequelSymbol _) a) =
    show s `mappend` " = " `mappend` show a
  showSingleSet (SequelRelationalOperation Equal s@SequelSymbolOperation{} a) =
    show s `mappend` " = " `mappend` show a
  showSet (Just cols) = intercalate "," $ fmap showSingleSet cols

  showInsertCols Nothing = ""
  showInsertCols (Just cols) =
    " (" `mappend`
      intercalate ", " (fmap (show.SequelColumn) cols) `mappend`
    ")"

  showLimit Nothing = ""
  showLimit (Just (a,0)) = " LIMIT " `mappend` show a
  showLimit (Just (a,b)) = " LIMIT " `mappend` show a `mappend` " OFFSET " `mappend` show b

  showSimpleLimit Nothing = ""
  showSimpleLimit (Just (a, _)) = " LIMIT " `mappend` show a

  showVals (Just cols) =
    intercalate ", " $
      fmap (\ row ->
        "(" `mappend`
        intercalate ", " (fmap show row) `mappend`
        ")"
      )
      cols

  showUpsert SequelUpsertEmpty _ _ = ""
  showUpsert (SequelUpsertKey k) (Just fields) (SequelTable (SequelSymbol tbname)) = "ON CONFLICT ON CONSTRAINT " `mappend`
                                                                                      show k `mappend`
                                                                                      " DO UPDATE SET" `mappend` intercalate "," (fmap showUpdateValue fields)
  showUpsert (SequelUpsertField k) (Just fields) (SequelTable (SequelSymbol tbname)) = "ON CONFLICT (" `mappend`
                                                                                      show k `mappend`
                                                                                      ") DO UPDATE SET" `mappend` intercalate ", " (fmap showUpdateValue fields)
  showUpsert SequelUpsertAuto (Just fields) _ = " ON DUPLICATE KEY UPDATE " `mappend`  intercalate ", " ( fmap ( \f -> show f  `mappend` "=VALUES(" `mappend` show f `mappend` ")" ) fields)

  showUpdateValue field = show field `mappend` " = EXCLUDED." `mappend` show field


  plainQuery = PlainQuery

  showQuery (SequelQuery SELECT cols table _ cond order groupBy having lim _) =
    "SELECT " `mappend`
    showCols cols `mappend`
    " FROM " `mappend`
    show table `mappend`
    showCond cond `mappend`
    showOrder order `mappend`
    showGroup groupBy `mappend`
    showHaving having `mappend`
    showLimit lim

  showQuery (PlainQuery s) = s

  showQuery (SequelQuery UPDATE cols table _ cond _ _ _ lim _) =
    "UPDATE " `mappend`
    show table `mappend`
    " SET " `mappend`
    showSet cols `mappend`
    showCond cond `mappend`
    showSimpleLimit lim



  showQuery (SequelQuery INSERT cols table vals _ _ _ _ _ upsert) =
    "INSERT INTO " `mappend`
    show table `mappend`
    showInsertCols cols `mappend`
    " VALUES " `mappend`
    showVals vals `mappend`
    showUpsert upsert cols table


  showQuery (SequelQuery DELETE _ table _ cond _ _ _ lim _) =
    "DELETE FROM " `mappend`
    show table `mappend`
    showCond cond `mappend`
    showSimpleLimit lim

  instance Show SequelQuery where
    show = showQuery

  t s@(SequelSymbol _) = SequelTable s
  t s@SequelSymbolOperation{} = SequelTable s
  ts = SequelTable . SequelSymbol
  s = SequelSymbol
  don'tEscape = SequelString
  f = SequelFunctor
  currentTimestamp = CurrentTimeStamp

  vdef = SequelDefault

  class SequelValue a where
    v :: a -> SequelExpression

  instance SequelValue T.Text where
     v s = SequelString escaped
      where
        s' =  T.unpack . escapeText $ s
        escaped = "'" `mappend` s' `mappend` "'"

  instance SequelValue TL.Text where
     v s = SequelString escaped
      where
        s' =  TL.unpack . escapeTextLazy $ s
        escaped = "'" `mappend` s' `mappend` "'"

  instance SequelValue BS.ByteString where
     v s = SequelString escaped
      where
        s' = BS.unpack . escapeByteString $ s
        escaped = "'" `mappend` s' `mappend` "'"

  instance SequelValue String where
    v s = SequelString escaped
      where
        s' = escapeString s
        escaped = quoteString:s' `mappend` [quoteString]

  instance SequelValue Int where
    v = SequelIntegral

  instance SequelValue Double where
    v = SequelNumber

  instance SequelValue Bool where
    v = SequelBool

  escapeString s = s

  escapeText s =
    let s' = T.replace "\\" "\\\\" s
    in T.replace "'" "\\'" s'

  escapeTextLazy s =
    let s' = TL.replace "\\" "\\\\" s
    in TL.replace "'" "\\'" s'

  escapeByteString s = s
    --let s' = BS.replace $ s "\\" "\\\\"
    --in BS.replace s' "'" "\\'"
