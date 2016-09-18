module MiniSequel.Mapper 
where
  import Database.HDBC
  class SequelMapper a where
    fromQuery :: [SqlValue] -> a
