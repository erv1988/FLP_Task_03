{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Exit
import System.IO
import Prelude hiding (catch)
import System.Directory
import Control.Exception
import System.IO.Error hiding (catch)
import System.Process 
import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import           Data.String (IsString(..))


{-
Подготовка:
cabal update
cabal install sqlite-simple

Уже есть команды:
:0 - подготовка БД
:r - показать содердимое таблицы test
:i - добавить строку в таблицу test
:q - выйти из программы

Пробный запуск:
cabal run
> :0
> :r
(1,"test string")
> :iteststring 2
> :iteststring 3
> :r
(1,"test string")
(2,"teststring 2")
(3,"teststring 3")
> :q



Доделать проект:
Добавить  и изменить обработчики в process, которые
- позволяют создавать и удалять таблицы;
- позволяют получить список таблиц
- позволяют выбрать данные с выбранной таблицы
- позволяют вывести потаблично всю информацию с БД
- выполнить SQL-запрос из файла

-}


----------------------------------------------------------------------------------------------
-- Основной цикл программы
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    loop
    where
        loop = do
            putStr "> " 
            line <- getLine
            success <- process (splitAt 2 line)
            if success 
                then loop
                else exitWith ExitSuccess 

-- Имя тестовой базы
dbName = "db.sqlite"
testTable = "test"

----------------------------------------------------------------------------------------------
process :: (String,String) -> IO Bool
-- выход из программы
process (":q",_) = return False

-- настроить проект с нуля
process (":0",_) = do
    -- если есть бд - удалить ее
    removeIfExists dbName
    -- создать таблицу тест
    system $ "sqlite3 " ++ dbName ++ " \"CREATE TABLE "++ testTable ++ " (id INTEGER PRIMARY KEY, str text);INSERT INTO test (str) VALUES ('test string');\""
    return True

-- посмотреть таблицу test
process (":r",_) = do
    conn <- open dbName
    r <- query_ conn (fromString ("select * from "++testTable++";")) :: IO [(Int,String)]
    mapM_ print r
    close conn
    return True

-- вставка строки
process (":i",str) = do
    conn <- open dbName
    execute_ conn (fromString $ "insert into "++testTable++" (str) values ('" ++ str ++ "');")
    close conn
    return True

-- для прочих случаев
process x = do
    putStrLn $ "user input: " ++ show x
    return True

----------------------------------------------------------------------------------------------
-- вспомогательные функции
removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e
----------------------------------------------------------------------------------------------

