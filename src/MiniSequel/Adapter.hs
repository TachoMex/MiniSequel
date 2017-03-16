module MiniSequel.Adapter

where
  import MiniSequel
  import MiniSequel.Model
  import MiniSequel.Expression

  data SequelAdapterDriver = MySQLDriver | PostgreSQLDriver
  data SequelAdapterConfig = SequelAdapterConfig {
    driver :: SequelAdapterDriver,
    host :: String,
    port :: String,
    user :: String,
    password :: String,
    database :: String
  }
