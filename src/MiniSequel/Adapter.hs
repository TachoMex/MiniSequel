module MiniSequel.Adapter

where
  import MiniSequel
  import MiniSequel.Model
  import MiniSequel.Expression
  import MiniSequel.Mapper

  import qualified Database.HDBC as DB
  import qualified Database.HDBC.MySQL as MSQL
  import qualified Database.HDBC.PostgreSQL as PSQL

  data SequelAdapterDriver = MySQLDriver | PostgreSQLDriver deriving (Show)

  data SequelAdapterConfig = SequelAdapterConfig {
    driver :: SequelAdapterDriver,
    host :: String,
    port :: Int,
    user :: String,
    password :: String,
    database :: String,
    maxConnections :: Int
  }

  mySQLConnector conf =
    MSQL.connectMySQL MSQL.defaultMySQLConnectInfo {
      MSQL.mysqlHost       = host conf,
      MSQL.mysqlUser       = user conf,
      MSQL.mysqlPassword   = password conf,
      MSQL.mysqlPort       = port conf,
      MSQL.mysqlDatabase   = database conf
    }
