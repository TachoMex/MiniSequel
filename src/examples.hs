{-# LANGUAGE OverloadedStrings #-}
import MiniSequel
import MiniSequel.Expression
import MiniSequel.Model
import MiniSequel.Mapper
import MiniSequel.Adapter
import Data.ByteString.Char8 (ByteString, unpack)
import Database.HDBC.MySQL
import Database.HDBC.Types
import Database.HDBC

sample_select =
  select [users ~> name, age, age *. n 3 =: s"3_ages"] $
  where' (users ~> name =. v"somebody" &&. age >=. n 18) $
  from $ t users
  where
    users = s "users"
    name = s "name"
    age = s "age"
    id = s "id"

sample_update =
  update [users ~> name =. v"tacho", age =. n 22] $
  where' (users ~> name =. v"somebody" &&. age >=. n 18) $
  from $ t users
  where
    users = s "users"
    name = s "name"
    age = s "age"
    id = s "id"

sample_order =
  select [users ~> name, age, age *. n 3 =: s"3_agees"] $
  where' (users ~> name =. v"somebody" &&. age >=. n 18) $
  order_by [Asc (name), Desc (age >=. n 18), Asc (f LENGTH [age])] $
  from $ t users
  where
    users = s "users"
    name = s "name"
    age = s "age"
    id = s "id"

sample_insert =
  insert [id, name, age] $
  values [
    [SequelNull, v"alice", n 22],
    [SequelNull, v"bob", n 21],
    [SequelNull, v"cheryl", n 22]
    ] $
  into $ t users
    where
      users = s "users"
      id = s "id"
      name = s "name"
      age = s "age"

sample_delete =
  delete $
  where' (s"name" =. v"tacho") $
  from (ts "users")

full_test =
  select [name, age, user, register_date, f MAX [salary] =: salary] $
  where' (f YEAR [register_date] >. v"2015-12-31") $
  group_by [name] $
  order_by [Desc salary] $
  having (salary >. n 10000) $
  from $ t employee
    where
      name          = s"name"
      age           = s "age"
      user          = s "user"
      register_date = s "register_date"
      salary        = s "salary"
      employee      = s "employee"

sample_join = ""

data User = User {
  user_id :: Int,
  name :: ByteString,
  username :: ByteString,
  email :: ByteString,
  password :: ByteString
} deriving (Show)

instance SequelModel User where
  create_model = table (s"user") [
    primary_key $ auto_increment $ not_null $ column (s"user_id")        SequelInteger,
                                   not_null $ column (s"name")          (SequelVarchar 50),
                          unique $ not_null $ column (s"username")      (SequelVarchar 30),
                                   not_null $ column (s"email")         (SequelVarchar 60),
                                   not_null $ column (s"password")      (SequelVarchar 1050)]

instance SequelMapper User where
  from_sql_row [
    SqlInt32 uid',
    SqlByteString name',
    SqlByteString user',
    SqlByteString mail',
    SqlByteString pass] = User {
      user_id = fromIntegral uid',
      name = name',
      username = user',
      email = mail',
      password = pass
    }

  create (User id' name' user mail pass) =
    insert [ s"user_id", s"name", s"username", s"email", s"password"] $
    values [[SequelNull, v $ unpack name', v $unpack user,v$ unpack mail, v$ unpack pass]] $
    into $ ts "user"

find_by_username :: (IConnection a) => a -> String -> IO User
find_by_username con user = do
  let query = first $ where' (s"username" =. v user) $ from $ ts "user"
  result <- exec con query
  return $ from_sql_row $ head result :: IO User

main = do
  print sample_select
  print sample_update
  print sample_order
  print sample_insert
  print sample_delete
  print full_test
  print sample_join
  print (create_model :: Model User)
  print $ create you
  where
    you = User {
      user_id = 8,
      name = "tacho",
      username = "tacho",
      email = "tachoguitar@gmail.com",
      password = "4G00dP455vv0|^d"
    }