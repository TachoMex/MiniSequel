{-# LANGUAGE FlexibleContexts #-}
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
    fromSqlRow :: [SqlValue] -> a

    create :: a -> SequelQuery
    create a = query
      where
        fields = map (s.snakeCase) $ constrFields .toConstr $ a
        row_val = map don'tEscape $ stringValues a
        query = insert fields $ values [row_val] $ into table_name
        table_name = ts $ snakeCase  $ show $ toConstr a

    createMulti :: [a] -> SequelQuery
    createMulti a = query
      where
        fields = map (s.snakeCase) $ constrFields . toConstr $ head a
        vals = map createValues a
        query = insert fields $ values vals $ into table_name
        table_name = ts $ snakeCase  $ show $ toConstr $  head a
        createValues row = map don'tEscape $ stringValues row

    store :: a -> SequelQuery
    store a = query
      where
        key:fields = map (s.snakeCase) $ constrFields . toConstr $ a
        id : vals = map don'tEscape $ stringValues a
        query = update set_fields $
                where' (key =. id) $
                from table_name
        table_name = ts $ snakeCase  $ show $ toConstr a
        set_fields = zipWith (=.) fields vals

  snakeCase :: String -> String
  snakeCase = map toLower . concat . underscores . splitR isUpper
    where
      underscores :: [String] -> [String]
      underscores [] = []
      underscores (h:t) = h : map ('_':) t
      splitR :: (Char -> Bool) -> String -> [String]
      splitR _ [] = []
      splitR p s =
        let
          go :: Char -> String -> [String]
          go m s' = case break p s' of
            (b', [])     -> [ m:b' ]
            (b', x:xs) -> ( m:b' ) : go x xs
        in case break p s of
          (b,  [])    -> [ b ]
          ([], h:t) -> go h t
          (b, h:t) -> b : go h t
