module MiniSequel.Adapters.Adapter

where
  import Data.Map as Map
  import Database.HDBC.SqlValue
  import MiniSequel
  import MiniSequel.Model
  import Database.HDBC

  --type SequelRow = (Map.Map String ByteString)

  exec :: (IConnection c) => c -> SequelQuery -> (IO [[SqlValue]])
  exec con query = quickQuery' con (show query) []

  take_model :: (IConnection c) => c -> Model b -> (IO Integer)
  take_model con model = run con (show model) []

  --values_as_string ::[String] -> [[SqlValue]] -> SequelRow
  --values_as_string ::