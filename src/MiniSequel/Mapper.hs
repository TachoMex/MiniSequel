module MiniSequel.Mapper
where
  import MiniSequel
  import Database.HDBC
  class SequelMapper a where
    from_sql_row :: [SqlValue] -> a
    store :: a -> SequelQuery
    update :: a -> SequelQuery