module MiniSequel.Adapter.MySQL
  where
    import MiniSequel
    import MiniSequel.Model
    import Data.Map as Map
    import Database.HDBC
    import Database.HDBC.SqlValue


    instance Show SequelQuery where
      show = showQuery '`' '\''

    instance (Show a) => Show (Model a) where
      show = showModel '`' '\''


    exec :: (IConnection c) => c -> SequelQuery -> IO [[SqlValue]]
    exec con query
      | _queryType query == SELECT = quickQuery' con (show query) []
      | otherwise =
        do
          result <- run con (show query) []
          return [[toSql result]]

    takeModel :: (IConnection c, Show b) => c -> Model b -> IO Integer
    takeModel con model = run con (show model) []
