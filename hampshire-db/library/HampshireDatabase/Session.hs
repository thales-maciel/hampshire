module HampshireDatabase.Session where

import HampshireDatabase.Prelude
import Hasql.Session (Session)
import qualified Hasql.Transaction as Transaction
import qualified HampshireDatabase.Transaction as Transaction
import Hasql.Transaction.Sessions (Mode(Read, Write), IsolationLevel (Serializable), transaction)

insertPerson :: (UUID, Text, Text, Day, Maybe (Vector Text)) -> Session ()
insertPerson person = transaction Serializable Write $ Transaction.insertPerson person

getPerson :: UUID -> Session (Maybe (UUID, Text, Text, Day, Maybe (Vector Text)))
getPerson id = transaction Serializable Read $ Transaction.getPerson id

searchPeople :: Text -> Session (Vector (UUID, Text, Text, Day, Maybe (Vector Text)))
searchPeople term = transaction Serializable Read $ Transaction.searchPeople term

countPeople :: Session Text
countPeople = transaction Serializable Read Transaction.countPeople
