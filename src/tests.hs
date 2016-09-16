import MiniSequel
import MiniSequel.Expression
import MiniSequel.Model
import MiniSequel.Adapters.Adapter
import Database.HDBC.MySQL

test_select = 
  select [users ~> nombre, edad, edad *. n 3 =: s"3_edades"] $ 
  where' (users ~> nombre =. v"pony" &&. edad >=. n 18) $ 
  from $ t users
  where 
    users = s "users"
    nombre = s "nombre"
    edad = s "edad"
    id = s "id"

test_update = 
  update [users ~> nombre =. v"tacho", edad =. n 22] $ 
  where' (users ~> nombre =. v"pony" &&. edad >=. n 18) $ 
  from $ t users
  where 
    users = s "users"
    nombre = s "nombre"
    edad = s "edad"
    id = s "id"

test_order = 
  select [users ~> nombre, edad, edad *. n 3 =: s"3_edades"] $ 
  where' (users ~> nombre =. v"pony" &&. edad >=. n 18) $ 
  order_by [Asc (nombre), Desc (edad >=. n 18), Asc (f LENGTH [edad])] $
  from $ t users
  where 
    users = s "users"
    nombre = s "nombre"
    edad = s "edad"
    id = s "id"

test_insert = 
  insert [id, nombre, edad] $
  values [
    [SequelNull, v"tacho", n 22],
    [SequelNull, v"lily", n 21],
    [SequelNull, v"bigui", n 22]
    ] $
  into $ t users
    where
      users = s "users"
      id = s "id"
      nombre = s "nombre"
      edad = s "edad"

test_delete =
  delete $ 
  where' (s"nombre" =. v"bigui") $
  from (ts "users")

full_test = 
  select [patente, pedimento, seccionaduanera, fraccion, f MAX [preciounitario] =: preciounitario] $
  where' (f YEAR [fechapagoreal] >. v"2015-12-31") $
  group_by [fraccion] $
  order_by [Desc preciounitario] $
  having (preciounitario >. n 10000) $
  from $ t _551 
    where
      _551 = s"_551"
      patente = s"patente"
      pedimento = s"pedimento"
      seccionaduanera = s"seccionaduanera"
      fraccion = s"fraccion"
      preciounitario = s "preciounitario"
      fechapagoreal = s "fechapagoreal"


test_join = 
  select [cliente ~> nombre =: cliente, 
          usuarios ~> nombre =: usuario, 
          actividad, 
          inicio, 
          fin] $
  where' (fin <>. v"0000-00-00") $ 
  order_by [Desc fin] $
  from $
    inner_join 
      (t cliente) 
      (inner_join 
          (t tiempo) 
          (ts "usuarios") 
          (tiempo ~> id_usuario =. usuarios ~> id)) 
      (tiempo ~> id_cliente =. cliente ~> id)
  where
    tiempo = s "tiempo"
    cliente = s "cliente"
    nombre = s "nombre"
    usuarios = s "usuarios"
    usuario = s "usuario"
    actividad = s "actividad"
    inicio = s "inicio"
    fin = s "fin"
    id_cliente = s "id_cliente"
    id_usuario = s "id_usuario"
    id = s "id" 

data Loan = Loan {
  loan_id :: String,
  user_id :: String, 
  status :: String,
  application_date :: String, 
  amount :: Maybe Double, 
  interests :: Maybe Double, 
  days :: Maybe Int
} 

instance SequelModel Loan where
  create_model = table (s"loans") [
    not_null $ primary_key $ auto_increment $ column (s"loan_id") SequelInteger,
    not_null $ column  (s"user_id") (SequelVarchar 30),
    not_null $ default' (v"pending") $ column (s"status") (SequelVarchar 30),
    not_null $ column (s"application_date") SequelDateTime,
    column (s"amount") SequelDouble,
    column (s"interests") SequelDouble,
    column (s"days") SequelInteger]

test_connection pass=  connectMySQL defaultMySQLConnectInfo {
  mysqlHost = "127.0.0.1",
  mysqlUser = "root",
  mysqlPassword = pass, 
  mysqlPort = 3306, 
  mysqlDatabase = "haskellSequel" 
}

create_models conn = do
  take_model conn $ if_not_exists $ (create_model :: Model Loan)