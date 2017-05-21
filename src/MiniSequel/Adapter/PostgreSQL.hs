module MiniSequel.Adapter.PostgreSQL
  where
    import MiniSequel
    import MiniSequel.Model
    import Data.Map as Map
    import Database.HDBC
    import Database.HDBC.SqlValue


    instance Show SequelQuery where
      show = showQuery '"' '\''

    instance (Show a) => Show (Model a) where
      show = showModel '"' '\''

    showM :: (Show a) => Model a -> String
    showM = show

    showQ :: SequelQuery -> String
    showQ = show
