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

user' = s "user"
users = s "users"
name' = s "name"
age = s "age"
id' = s "id"


sampleSelect =
  select [users ~> name', age, age *. ni 3 =: s"3_ages"] $
  where' (users ~> name' =. v"somebody" &&. age >=. ni 18) $
  from $ t users

sampleUpdate =
  update [users ~> name' =. v"tacho", age =. ni 22] $
  where' (users ~> name' =. v"somebody" &&. age >=. ni 18) $
  from $ t users

sampleOrder =
  select [users ~> name', age, age *. ni 3 =: s"3_agees"] $
  where' (users ~> name' =. v"somebody" &&. age >=. ni 18) $
  orderBy [Asc name', Desc (age >=. ni 18), Asc (f LENGTH [age])] $
  from $ t users

sampleInsert =
  insert [id', name', age] $
  values [
    [SequelNull, v"alice", ni 22],
    [SequelNull, v"bob", ni 21],
    [SequelNull, v"cheryl", ni 22]
    ] $
  into $ t users


sampleDelete =
  delete $
  where' (s"name" =. v"tacho") $
  from (ts "users")

fullTest =
  select [name', age, user', register_date, f MAX [salary] =: salary] $
  where' (f YEAR [register_date] >. v"2015-12-31") $
  groupBy [name'] $
  orderBy [Desc salary] $
  having (salary >. ni 10000) $
  from $ t employee
    where
      register_date = s "register_date"
      salary        = s "salary"
      employee      = s "employee"

sampleJoin = ""

data UserModel = UserModel {
  userId :: Int,
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
  createModel = table (s"user") [
    primaryKey $ autoIncrement $ notNull $ column (s"user_id")        SequelInteger,
                                 notNull $ column (s"name")          (SequelVarchar 50),
                        unique $ notNull $ column (s"username")      (SequelVarchar 30),
                                 notNull $ column (s"email")         (SequelVarchar 60),
                                 notNull $ column (s"password")      (SequelVarchar 1050)]

instance SequelMapper UserModel where
  fromSqlRow row = UserModel {
      userId = fromSql $ head row,
      name = fromSql $ row !! 1,
      username = fromSql $ row !! 2,
      email = fromSql $ row !! 3,
      password = fromSql $ row !! 4
    }


findByUserId :: (IConnection a) => a -> String -> IO UserModel
findByUserId con user = do
  let query = first $ where' (s"username" =. v user) $ from $ ts "user"
  result <- exec con query
  return $ fromSqlRow $ head result :: IO UserModel

main = do
  print sampleSelect
  print sampleUpdate
  print sampleOrder
  print sampleInsert
  print sampleDelete
  print fullTest
  print sampleJoin
  print (createModel :: Model UserModel)
  print $ create you
  print $ createMulti [you, you, you]
  print $ store you
  print (fromSqlRow [SqlInt32 10, SqlByteString "ImAUser", SqlByteString "ProUser", SqlByteString "mail@mail.com", SqlByteString "p4ssvv0rd"] :: UserModel)
  where
    you = UserModel {
      userId = 8,
      name = "tacho",
      username = "tacho",
      email = "tachoguitar@gmail.com",
      password = "4G00dP455vv0|^d"
    }