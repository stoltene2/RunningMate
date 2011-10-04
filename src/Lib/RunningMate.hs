
module RunningMate where


import Control.Applicative 
import Control.Monad

import Database.HDBC
import Database.HDBC.Sqlite3 (connectSqlite3)

data Run = Run { _name :: String
               , _meta :: Maybe RunMeta 
               } 
    deriving(Show, Eq)

data RunMeta = RunMeta { _id :: Integer }
    deriving (Show, Eq)

------------------------------------------------------------------------------
-- | MetaDB is a class that can extract Database MetaData from an
--  element.  Unique Id's, Creation Date, Modification Date, etc.
class MetaDB a where
    uid :: a -> Maybe Integer


instance MetaDB Run where
    -- uid for unique identifier
    uid r = _id <$> _meta r


------------------------------------------------------------------------------
-- | This is a class that handles saving records to some sort of connection
-- Maybe I don't want to put the Validation Constraint on DBSomething


class (Validation a) => DBSomething a where
    save :: (a -> ValidationFunList -> IO Bool)
    
------------------------------------------------------------------------------

-- ???? not sure about this
-- data ValidationFunction a = a -> Bool

-- class Validation a where
--     preCommit  :: [ValidationFunction a]
--     postCommit :: [ValidationFunction a]



createRun :: (IConnection connection) => connection -> Run -> IO Integer
createRun conn r = run conn "INSERT INTO run (name) VALUES (?)" [toSql $ _name r]


readRun :: [SqlValue] -> Either String Run
readRun xs = case xs of
               [i, n] -> Right $ Run (fromSql n) (Just $ RunMeta $ fromSql i)
               _      -> Left "Crap"

emptyRun :: Run
emptyRun = Run "" Nothing

ericRun :: Run
ericRun = Run "Eric" Nothing


main :: IO ()
main = do
    conn <- connectSqlite3 "test.db"
    val  <- run conn "CREATE TABLE run(id INTEGER PRIMARY KEY, name VARCHAR(100) NOT NULL)" []
    result <- createRun conn ericRun
    results <- quickQuery' conn "SELECT id,name FROM run" []
    let runs = map readRun results
    forM_ runs ( \val ->
                     case val of
                       Left err -> putStrLn err
                       Right x  -> do 
                                       putStrLn $ show x
                                       putStrLn $ "id: " ++ (maybe "" show $ uid x) ++ " -- " ++ (_name x)
               )
--  What happens if we try to insert emptyRun?
--  Can I easily create some sort of validation class
    disconnect conn

-- | ----------------------------------------------------------------------------
-- CREATE TABLE run(id INT NOT NULL AUTO_INCREMENT, PRIMARY KEY(id), name VARCHAR(100));