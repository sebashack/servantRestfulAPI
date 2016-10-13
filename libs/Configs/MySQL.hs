module Configs.MySQL
       (
         mysqlDevelConfig,
         mysqlProdConfig,
         mysqlTestConfig
       ) where
import qualified Database.Persist.MySQL as MySQL
import qualified Database.Persist.Sql as SQL
import qualified Database.MySQL.Base.Types as MySQLTypes
import Configs.ConfigTypes

-- Development Configuration

info :: MySQL.ConnectInfo
info = MySQL.ConnectInfo "localhost"
                         3306
                         "root"
                         "doncellasnoir9."
                         "destinos_devel"
                         [MySQLTypes.CharsetName "utf8"]
                         ""
                         Nothing


mysqlDevelConfig = ConfigMySQL info 1



-- TODO: Define a production configuration.

mysqlProdConfig = undefined

mysqlTestConfig = undefined
