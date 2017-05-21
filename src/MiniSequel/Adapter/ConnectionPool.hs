
{-# LANGUAGE ScopedTypeVariables #-}
module MiniSequel.Adapter.ConnectionPool
where
  import MiniSequel.Adapter
  import MiniSequel
  import Database.HDBC
  import Control.Concurrent.Chan
  import Control.Exception
  import Control.Monad (forM_)

  import Control.Monad.State (runState)
  import qualified MiniSequel.Adapter.MySQL as MSQL
  import qualified MiniSequel.Adapter.PostgreSQL as PSQL


  data ConnectionPool c = ConnectionPool {
    conf :: SequelAdapterConfig,
    pool :: Chan c
  }

  sequelPool :: (IConnection c) => SequelAdapterConfig -> (SequelAdapterConfig -> IO c) -> IO (ConnectionPool c)
  sequelPool conf connectFunc = do
    let poolSize = maxConnections conf
    cons <- newChan
    forM_ [1..poolSize] $ \ _ -> do
      x <- connectFunc conf
      writeChan cons x
    return $ ConnectionPool conf cons

  execQuery :: (IConnection c) => ConnectionPool c -> SequelTable -> Query -> IO [[SqlValue]]
  execQuery cons table query = do
    let ((), q) = runState query (from table)
        showFunc = case driver $ conf cons of
          MySQLDriver -> MSQL.showQ
          PostgreSQLDriver -> PSQL.showQ
    con <- readChan $ pool cons
    result <- try $ exec con q showFunc


    writeChan (pool cons) con
    case result of
      Left (ex :: SomeException) -> throwIO ex
      Right val -> return val

  exec :: (IConnection c) => c -> SequelQuery -> (SequelQuery -> String) -> IO [[SqlValue]]
  exec con query showF
    | _queryType query == SELECT = quickQuery' con (showF query) []
    | otherwise =
      do
        result <- withTransaction con (\ con -> run con (showF query) [])
        return [[toSql result]]



  -- query :: (IConnection c) =>  ConnectionPool c -> Query -> IO [Map String Value]
  -- query
  --
  --
  -- runTransaction :: ConnectionPool -> SequelTable ->(c -> IO ()) -> IO ()
  -- runTransaction _  _ = return ()
  --
  -- runQuery :: (IConnection c) => c -> SequelQuery -> IO a
  -- runQuery c q =
  --
  -- getTables :: (DB.IConnection c) => c -> IO [String]
  -- getTables = DB.getTables

  -- isEmpty ::
  --
  -- firstResutl
  --
  -- doQuery :: (DB.IConnection c) => c -> ()
  --
  -- runMigration :: (DB.IConnection c)
  --
  -- onSession :: (DB.IConnection c) => c ()
