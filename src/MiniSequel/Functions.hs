module MiniSequel.Functions
where
  import MiniSequel.Expression

  now = SequelFunctor NOW []
  max a = SequelFunctor MAX [a]
  min a = SequelFunctor MIN [a]
  avg a = SequelFunctor AVG [a]
  sum' a = SequelFunctor SUM [a]
  month a = SequelFunctor MONTH [a]
  if' cond a b = SequelFunctor IF [cond, a, b]
