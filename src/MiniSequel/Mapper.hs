{-# LANGUAGE DeriveDataTypeable
            ,FlexibleContexts #-}
module MiniSequel.Mapper
where
  import MiniSequel
  import MiniSequel.Expression ((=.))
  import Database.HDBC
  import Data.Data
  import Data.Char (toUpper, toLower, isUpper)
  import qualified GHC.Generics as G
  import qualified Generics.SOP as SOP

  stringValues :: (SOP.Generic a, SOP.All2 Show (SOP.Code a)) => a -> [String]
  stringValues a = SOP.hcollapse (SOP.hcmap
                                            (Proxy :: Proxy Show)
                                            (\ (SOP.I x) -> SOP.K (show x))
                                            (SOP.from a))

  class (G.Generic a, Data a, SOP.Generic a, SOP.All2 Show (SOP.Code a)) => SequelMapper a where
    from_sql_row :: [SqlValue] -> a

    create :: a -> SequelQuery
    create a = query
      where
        fields = map s $ constrFields .toConstr $ a
        row_val = map don't_escape $ stringValues a
        query = insert fields $ values [row_val] $ into table_name
        table_name = ts $ snake_case  $ show $ toConstr $ a

    create_multi :: [a] -> SequelQuery
    create_multi a = query
      where
        fields = map s $ constrFields . toConstr $ head a
        vals = map (\row -> map don't_escape $ stringValues row) a
        query = insert fields $ values vals $ into table_name
        table_name = ts $ snake_case  $ show $ toConstr $  head a

    store :: a -> SequelQuery
    store a = query
      where
        key:fields = map s $ constrFields . toConstr $ a
        id : vals = map don't_escape $ stringValues a
        query = update set_fields $
                where' (key =. id) $
                from $ table_name
        table_name = ts $ snake_case  $ show $ toConstr $ a
        set_fields = zipWith (=.) fields vals

  snake_case :: String -> String
  snake_case = (map toLower) . concat . underscores . split_r isUpper
    where
      underscores :: [String] -> [String]
      underscores [] = []
      underscores (h:t) = h : map ('_':) t
      split_r :: (Char -> Bool) -> String -> [String]
      split_r _ [] = []
      split_r p s =
        let
          go :: Char -> String -> [String]
          go m s' = case break p s' of
            (b', [])     -> [ m:b' ]
            (b', (x:xs)) -> ( m:b' ) : go x xs
        in case break p s of
          (b,  [])    -> [ b ]
          ([], (h:t)) -> go h t
          (b, (h:t)) -> b : go h t
