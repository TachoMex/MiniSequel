{-# LANGUAGE ScopedTypeVariables #-}
import Control.Monad.State
import MiniSequel
import MiniSequel.Expression
import MiniSequel.Functions
import MiniSequel.Adapter
import MiniSequel.Adapter.ConnectionPool
import Control.Monad.Trans (liftIO, lift)
import Database.HDBC
import Database.HDBC.MySQL
import Control.Concurrent

main :: IO ()
main = do
  putStrLn ":v"
  db <- sequelPool conf mySQLConnector :: IO (ConnectionPool Connection)
  threads <- forM [1..1000] $ \ (x :: Int) -> do
    threadId <- myThreadId
    execQuery db (ts "loans") $ do
       insert [s"loan_id", s"user_id", s"date"]
       values [[v $ show threadId, v x, now]]
  x <- execQuery db (ts "loans") first
  print x
  where
    conf = SequelAdapterConfig {
      driver = MySQLDriver,
      host = "127.0.0.1",
      port = 3306,
      user = "root",
      password = "IAmNotRoot!!",
      database = "test",
      maxConnections = 10
    }
