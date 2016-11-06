module MiniSequel.Adapter

where
  import Data.Map as Map
  import Database.HDBC.SqlValue
  import MiniSequel
  import MiniSequel.Model
  import MiniSequel.Expression
  import Database.HDBC
  import Database.HDBC.MySQL(withRTSSignalsBlocked)


  --type SequelRow = (Map.Map String ByteString)

  exec :: (IConnection c) => c -> SequelQuery -> IO [[SqlValue]]
  exec con query
    | _queryType query == SELECT = withRTSSignalsBlocked $ quickQuery' con (show query) []
    | otherwise =
      do
        withRTSSignalsBlocked $ run con (show query) []
        return [[SqlNull]]

  takeModel :: (IConnection c) => c -> Model b -> IO Integer
  takeModel con model = withRTSSignalsBlocked $ run con (show model) []
