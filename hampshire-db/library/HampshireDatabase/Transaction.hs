module HampshireDatabase.Transaction where

import HampshireDatabase.Prelude hiding (Category)
import Hasql.Transaction
import qualified HampshireDatabase.Statement as Statement
import qualified Hasql.Session as Session
import Hasql.Transaction.Sessions (Mode(Write), IsolationLevel (Serializable))
import Data.UUID.V4 (nextRandom)
import Data.Vector


insertPerson :: (UUID, Text, Text, Day, Maybe (Vector Text)) -> Transaction ()
insertPerson params =
  statement params Statement.insertPerson

getPerson :: UUID -> Transaction (Maybe (UUID, Text, Text, Day, Maybe (Vector Text)))
getPerson id =
  statement id Statement.getPerson

searchPeople :: Text -> Transaction (Vector (UUID, Text, Text, Day, Maybe (Vector Text)))
searchPeople term =
  statement term Statement.searchPeople

countPeople :: Transaction Text
countPeople =
  statement () Statement.countPeople

