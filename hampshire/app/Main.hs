{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types.Status
import Network.HTTP.Types.Method

import Hasql.Pool
import Hasql.Migration
import Hasql.Session (QueryError(..), ResultError(..), CommandError(..), CommandError)
import Hasql.Connection (settings)
import Hasql.Transaction.Sessions (Mode(Read, Write), IsolationLevel (Serializable), transaction)

import Data.Vector as V (toList)
import Data.Text (pack)

import Data.Aeson (FromJSON, ToJSON, decode, encode)

import Data.UUID (fromText, toText)
import Blaze.ByteString.Builder (copyByteString)
import qualified Data.ByteString.Char8 as BU

import HampshireDatabase.Session (getPerson, countPeople, insertPerson, searchPeople)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.ByteString.Lazy (fromStrict)
import HampshireDatabase.Utils (nextId)

data PersonOut = PersonOut
    { id         :: Text
    , nome       :: Text
    , apelido    :: Text
    , nascimento :: Text
    , stack      :: Maybe (Vector Text)
    } deriving (Show, Generic)
instance ToJSON PersonOut

data Person = Person
    { nome       :: Text
    , apelido    :: Text
    , nascimento :: Day
    , stack      :: Maybe (Vector Text)
    } deriving (Show, Generic)
instance FromJSON Person

main :: IO ()
main = do
    maybePort <- lookupEnv "PORT"
    let port = fromMaybe 8080 (maybePort >>= readMaybe)
    let db_config = settings "localhost" 5432 "postgres" "password" "postgres"
    pool <- acquire 16 (secondsToDiffTime 5) (secondsToDiffTime 5) (secondsToDiffTime 5) db_config
    putStrLn "Loading migrations"
    migration <- loadMigrationFromFile "V1__initial_setup.sql" "./migrations/V1__initial_setup.sql"
    putStrLn "Running migrations"
    _ <- use pool $ transaction Serializable Write $ runMigration MigrationInitialization
    res <- use pool $ transaction Serializable Write $ runMigration migration
    case res of
        Left e -> do
            putStrLn $ "Error migrating" ++ show e
            exitWith (ExitFailure 1)
        Right _ -> do
            putStrLn $ "Listening on port " ++ show port
            run port $ application pool

application :: Pool -> Request -> (Response -> IO b) -> IO b
application pool req res = 
    case (requestMethod req, pathInfo req) of
        ("POST", ["pessoas"])          -> handleCreate pool req res
        ("GET",  ["pessoas"])          -> handleSearch pool req res
        ("GET",  ["pessoas", id])      -> handleGet pool id req res
        ("GET",  ["contagem-pessoas"]) -> handleCount pool req res
        _                              -> handleDefault res


handleGet :: Pool -> Text -> Request -> (Response -> IO b) -> IO b
handleGet pool id _ res = do
    let maybePersonId = fromText id
    case maybePersonId of
        Nothing -> res $ responseLBS status404 [("Content-Type", "text/plain")] "Not found"
        Just personId -> do
            result <- use pool $ getPerson personId
            case result of
                Left _ -> res $ responseLBS status500 [("Content-Type", "text/plain")] "Internal Server Error"
                Right (Just p) -> do
                    let personResponse = personFromDb p
                    res $ responseLBS status200 [("Content-Type", "application/json")] $ encode personResponse
                Right Nothing -> res $ responseLBS status404 [("Content-Type", "text/plain")] "Not found"

handleCreate :: Pool -> Request -> (Response -> IO b) -> IO b
handleCreate pool req res = do
    body <- consumeRequestBodyLazy req
    case decode body :: Maybe Person of
        Nothing     -> res $ responseLBS status422 [("Content-Type", "text/plain")] "Unprocessable Entity"
        Just Person{ nome, apelido, nascimento, stack } -> do
            personId <- nextId
            let sess = insertPerson (personId, nome, apelido, nascimento, stack)
            result <- use pool sess
            case result of
                Left (SessionUsageError (QueryError _ _ e)) -> case e of 
                    ResultError (ServerError "23505" _ _ _ _) -> res $ responseLBS status422 [("Content-Type", "text/plain")] "Unprocessable Entity"
                    ResultError (ServerError "23000" _ _ _ _) -> res $ responseLBS status422 [("Content-Type", "text/plain")] "Unprocessable Entity"
                    _ -> res $ responseLBS status500 [("Content-Type", "text/plain")] "Internal Server Error"
                Left _ -> res $ responseLBS status500 [("Content-Type", "text/plain")] "Internal Server Error"
                Right _ -> do
                    let locationUrl = encodeUtf8 $ pack $ "/pessoas/" ++ show personId
                    res $ responseLBS status201 [("Content-Type", "text/plain"), ("Location", locationUrl)] "Created"

handleSearch pool req res = do
    let searchTerm = getTermFromQuery $ queryString req
    case searchTerm of
        Nothing   -> res $ responseLBS status400 [("Content-Type", "text/plain")] "Missing query parameter t"
        Just term -> do
            result <- use pool $ searchPeople $ decodeUtf8 term
            case result of
                Left _ -> res $ responseLBS status500 [("Content-Type", "text/plain")] "Internal Server Error"
                Right people -> do
                    let personResponse = fmap personFromDb people
                    res $ responseLBS status200 [("Content-Type", "application/json")] $ encode personResponse
            

handleCount :: Pool -> Request -> (Response -> IO b) -> IO b
handleCount pool _ res = do
    result <- use pool countPeople
    case result of
        Left _ -> res $ responseLBS status500 [("Content-Type", "text/plain")] "Internal Server Error"
        Right count -> res $ responseLBS status200 [("Content-Type", "text/plain")] $ fromStrict $ encodeUtf8 count

handleDefault res = res $ responseLBS status404 [("Content-Type", "text/plain")] "Route not found"

personFromDb :: (UUID, Text, Text, Day, Maybe (Vector Text)) -> PersonOut
personFromDb (i, n, a, d, s) = 
    PersonOut
    { id = toText i
    , nome = n
    , apelido = a
    , nascimento = pack (iso8601Show d)
    , stack = s
    }

getTermFromQuery :: [(ByteString, Maybe ByteString)] -> Maybe ByteString
getTermFromQuery ((k, v): xs) = if k == "t" then v else getTermFromQuery xs
getTermFromQuery [] = Nothing
