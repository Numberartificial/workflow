import Hasql.Connection (Connection, Settings, connectionSettings, acquire, release)
import Hasql.Session (Session, statement, run)
import Hasql.Statement (Statement(..))
import Hasql.Decoders (rowVector, column)
import Hasql.Encoders (unit)
import Data.Int (Int32)
import Data.Vector (Vector)
import Data.Text (Text)
import Data.ByteString.Char8 (pack)

-- ClickHouse connection settings
settings :: Settings
settings = connectionSettings 8123 "localhost" "default" "" "default"

-- SQL statement to fetch data
fetchData :: Statement () (Vector (Text, Int32))
fetchData = Statement "SELECT name, age FROM users" unit (rowVector (column text, column int4)) True

-- Function to fetch data from ClickHouse
fetchFromClickHouse :: IO (Either (Vector (Text, Int32)))
fetchFromClickHouse = do
  result <- acquire settings
  case result of
    Left err -> return (Left err)
    Right conn -> do
      data <- run (statement () fetchData) conn
      release conn
      return data