module MiniSequel.Adapter.MySQL.Log
where
  import qualified MiniSequel.Adapter.MySQL as Adapter

  exec con query = do
    print query
    Adapter.exec con query

  takeModel con model = do
    print model
    Adapter.takeModel con model
