module MiniSequel.Mapper
where
  import MiniSequel
  import Database.HDBC
  class SequelMapper a where
    from_sql_row :: [SqlValue] -> a
    create :: a -> SequelQuery
    store :: a -> SequelQuery
