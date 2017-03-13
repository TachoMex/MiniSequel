module MiniSequel.Adapter

where
  import Data.Map as Map
  import Database.HDBC.SqlValue
  import MiniSequel
  import MiniSequel.Model
  import MiniSequel.Expression
  import Database.HDBC


  --type SequelRow = (Map.Map String ByteString)

  exec :: (IConnection c) => c -> SequelQuery -> IO [[SqlValue]]
  exec con query
    | _queryType query == SELECT = quickQuery' con (show query) []
    | otherwise =
      do
        result <- run con (show query) []
        return [[toSql result]]

  takeModel :: (IConnection c) => c -> Model b -> IO Integer
  takeModel con model = run con (show model) []
