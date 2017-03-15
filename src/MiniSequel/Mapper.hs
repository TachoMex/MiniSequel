{-# LANGUAGE FlexibleContexts
           , UndecidableSuperClasses #-}
module MiniSequel.Mapper
where
  import MiniSequel
  import MiniSequel.Expression ((=.), SequelExpression)

  import Database.HDBC

  import Data.Data
  import Data.Char (toUpper, toLower, isUpper)
  import Data.Maybe (fromJust)

  import qualified GHC.Generics as G
  import qualified Generics.SOP as SOP

  sequelValues :: (SOP.Generic a, SOP.All2 SequelValue (SOP.Code a)) => a -> [SequelExpression]
  sequelValues a = SOP.hcollapse (SOP.hcmap
                                            (Proxy :: Proxy SequelValue)
                                            (\ (SOP.I x) -> SOP.K (v x))
                                            (SOP.from a))

  class SequelMapper a where
    fromSqlRow :: [SqlValue] -> a
    create :: a -> SequelQuery
    createMulti :: [a] -> SequelQuery
    createMulti a = query
      where
        fields = fromJust . _colums . create . head $ a
        vals = map (head . fromJust . _values . create) a
        query = makeQuery tableName $ do
              insert fields
              values vals
        tableName = _from . create . head $ a


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


  class (G.Generic a, Data a, SOP.Generic a, SOP.All2 SequelValue (SOP.Code a)) => SequelMapperUpdateable a where
    store :: a -> SequelQuery
    store a = query
      where
        key:fields = map (s.snakeCase) $ constrFields . toConstr $ a
        id : vals = sequelValues a
        query = undefined
          --  update set_fields $
          --       where' (key =. id) $
          --       from table_name
        table_name = ts $ snakeCase  $ show $ toConstr a
        set_fields = zipWith (=.) fields vals
