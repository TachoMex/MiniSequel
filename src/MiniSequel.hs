module MiniSequel(
)where
  import MiniSequel.Expression
  data SequelQuery = {
    _query_type :: String, 
    _from :: SequelTable
    _where' :: SequelExpression
    _order :: SequelOrder
    _group :: [SequelColumn]
    _having :: SequelExpression
  }

  
