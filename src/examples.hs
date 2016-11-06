{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}

import qualified GHC.Generics as G
import qualified Generics.SOP as SOP
import Data.Data
import Data.Typeable
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
  select [users ~> name, age, age *. ni 3 =: s"3_ages"] $
  where' (users ~> name =. v"somebody" &&. age >=. ni 18) $
  from $ t users
  where
    users = s "users"
    name = s "name"
    age = s "age"
    id = s "id"

sample_update =
  update [users ~> name =. v"tacho", age =. ni 22] $
  where' (users ~> name =. v"somebody" &&. age >=. ni 18) $
  from $ t users
  where
    users = s "users"
    name = s "name"
    age = s "age"
    id = s "id"

sample_order =
  select [users ~> name, age, age *. ni 3 =: s"3_agees"] $
  where' (users ~> name =. v"somebody" &&. age >=. ni 18) $
  order_by [Asc (name), Desc (age >=. ni 18), Asc (f LENGTH [age])] $
  from $ t users
  where
    users = s "users"
    name = s "name"
    age = s "age"
    id = s "id"

sample_insert =
  insert [id, name, age] $
  values [
    [SequelNull, v"alice", ni 22],
    [SequelNull, v"bob", ni 21],
    [SequelNull, v"cheryl", ni 22]
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
  having (salary >. ni 10000) $
  from $ t employee
    where
      name          = s"name"
      age           = s "age"
      user          = s "user"
      register_date = s "register_date"
      salary        = s "salary"
      employee      = s "employee"

sample_join = ""

data UserModel = UserModel {
  user_id :: Int,
  name :: ByteString,
  username :: ByteString,
  email :: ByteString,
  password :: ByteString
} deriving (Show
          , Typeable
          , G.Generic
          , Data)

instance  SOP.Generic UserModel

instance SequelModel UserModel where
  create_model = table (s"user") [
    primary_key $ auto_increment $ not_null $ column (s"user_id")        SequelInteger,
                                   not_null $ column (s"name")          (SequelVarchar 50),
                          unique $ not_null $ column (s"username")      (SequelVarchar 30),
                                   not_null $ column (s"email")         (SequelVarchar 60),
                                   not_null $ column (s"password")      (SequelVarchar 1050)]

instance SequelMapper UserModel where
  from_sql_row row = UserModel {
      user_id = fromSql $ row !! 0,
      name = fromSql $ row !! 1,
      username = fromSql $ row !! 2,
      email = fromSql $ row !! 3,
      password = fromSql $ row !! 4
    }


find_by_username :: (IConnection a) => a -> String -> IO UserModel
find_by_username con user = do
  let query = first $ where' (s"username" =. v user) $ from $ ts "user"
  result <- exec con query
  return $ from_sql_row $ head result :: IO UserModel

main = do
  print sample_select
  print sample_update
  print sample_order
  print sample_insert
  print sample_delete
  print full_test
  print sample_join
  print (create_model :: Model UserModel)
  print $ create you
  print $ create_multi [you, you, you]
  print $ store you
  print $ (from_sql_row [SqlInt32 10, SqlByteString "ImAUser", SqlByteString "ProUser", SqlByteString "mail@mail.com", SqlByteString "p4ssvv0rd"] :: UserModel)
  where
    you = UserModel {
      user_id = 8,
      name = "tacho",
      username = "tacho",
      email = "tachoguitar@gmail.com",
      password = "4G00dP455vv0|^d"
    }