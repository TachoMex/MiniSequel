{-# LANGUAGE DataKinds #-}
module MiniSequel
where
  import Data.List
  import MiniSequel.Expression
  data SequelQuery = SequelQuery {
    _query_type :: SequelQueryType, 
    _colums :: [SequelColumn],
    _from :: SequelTable,
    _where :: SequelExpression,
    _order :: SequelOrder,
    _group :: [SequelColumn],
    _having :: SequelExpression
  }

  data SequelColumn = SequelColumn SequelExpression | ALL
  data SequelQueryType = INSERT | DELETE | UPDATE | SELECT deriving (Show)
  data SequelTable = SequelTable String | SequelJoin SequelTable SequelTable SequelJoinType SequelExpression
  data SequelOrder = Asc [SequelColumn] | Desc [SequelColumn] | Unordered
  data SequelJoinType = INNER | LEFT | RIGHT deriving (Show)

  instance Show SequelColumn where
    show (SequelColumn sym@(SequelSymbol _)) = show sym
    show ALL = "*"

  instance Show SequelTable where
    show (SequelTable s) = s
    show (SequelJoin a b t expr) = show a ++ show " " ++ show t ++ " JOIN " ++ show b ++ show expr

  instance Show SequelOrder where
    show Unordered = ""
    show (Asc arr) = "ASC " ++ (intercalate "," $ map show arr)
 


  from table@(SequelSymbol _) = SequelQuery {
    _query_type = SELECT, 
    _colums = [ALL],
    _from = SequelTable $ show table, 
    _where = n 1, 
    _order = Unordered, 
    _group = [], 
    _having = n 1
  }

  instance Show SequelQuery where
    show (SequelQuery columns type' from' where' order' group' having') = 
      show columns ++ show type' ++ " FROM " ++ show from' ++ " where " ++ show where'++ show order' ++ show group' ++ show having'


