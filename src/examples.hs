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
import MiniSequel.Adapter.MySQL (exec, takeModel)
import Data.ByteString.Char8 (ByteString, unpack)
import Database.HDBC.MySQL
import Database.HDBC.Types
import Database.HDBC

user' = s "user"
users = s "users"
name' = s "name"
age = s "age"
id' = s "id"


sampleTable = from $ t users

sampleSelect = makeQuery (t users) $ do
  select [users ~> name', age, age *. vi 3 =: s"3_ages"]
  where' (users ~> name' =. v"somebody" &&. age >=. vi 18)

sampleUpdate = makeQuery (t users) $ do
  update [users ~> name' =. v"tacho", age =. v (22 :: Int)]
  where' (users ~> name' =. v"somebody" &&. age >=. v (18 :: Int))

sampleOrder = makeQuery (t users) $ do
  select [users ~> name', age, age *. vi 3 =: s"3_agees"]
  where' (users ~> name' =. v"somebody" &&. age >=. vi 18)
  orderBy [Asc name', Desc (age >=. vi 18), Asc (f LENGTH [age])]


sampleInsert = makeQuery (t users) $ do
  insert [id', name', age]
  values [
    [SequelNull, v"alice", vi 22],
    [SequelNull, v"bob", vi 21],
    [SequelNull, v"cheryl", vi 22]
    ]


sampleDelete = makeQuery (t users) $ do
  delete
  where' (s"name" =. v"tacho")

fullTest = makeQuery (t users) $ do
  select [name', age, user', register_date, f MAX [salary] =: salary]
  where' (f YEAR [register_date] >. v"2015-12-31")
  groupBy [name']
  orderBy [Desc salary]
  having (salary >. vf 10000)
    where
      register_date = s "register_date"
      salary        = s "salary"
      employee      = s "employee"

sampleJoin = makeQuery (t users) $ do
  select [name, age, contents, date]
  where' $ name =. v "tacho"
  innerJoin (t messages) $ using [userId]
  limit 1
  offset 1
  where
    name = s "name"
    contents = s "contents"
    date = s "date"
    messages = s "messages"
    userId = s "user_id"

data UserModel = UserModel {
  userId :: Int,
  name :: String,
  username :: String,
  email :: String,
  password :: String
} deriving (Show
          , Typeable
          , G.Generic
          , Data)

instance  SOP.Generic UserModel

instance SequelModel UserModel where
  createModel = table (s"user") [
    primaryKey . autoIncrement . notNull $ column (s"user_id")        SequelInteger,
                                 notNull $ column (s"name")          (SequelVarchar 50),
                        unique . notNull $ column (s"username")      (SequelVarchar 30),
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

  create u = makeQuery (ts "users") $ do
    insert [s"name", s"username", s"email", s"password"]
    values [[v $ name u, v $ username u, v $ email u, v $ password u]]




-- findByUserId :: (IConnection a) => a -> String -> IO UserModel
-- findByUserId con user = do
--   let query = first . where' (s"username" =. v user) $ from $ ts "user"
--   result <- exec con query
--   return $ fromSqlRow $ head result :: IO UserModel

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
  print (fromSqlRow [toSql (10 :: Int), toSql "ImAUser", toSql "ProUser", toSql "mail@mail.com", toSql "p4ssvv0rd"] :: UserModel)
  where
    you = UserModel {
      userId = 8,
      name = "tacho",
      username = "tacho",
      email = "tachoguitar@gmail.com",
      password = "4G00dP455vv0|^d"
    }
