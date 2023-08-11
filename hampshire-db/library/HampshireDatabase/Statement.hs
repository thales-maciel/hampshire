{-# LANGUAGE TemplateHaskellQuotes #-}

module HampshireDatabase.Statement where

import HampshireDatabase.Prelude
import Hasql.Statement
import qualified Hasql.Encoders as E
import qualified Hasql.Decoders as D
import Hasql.Encoders (noParams)
import Contravariant.Extras.Contrazip
import Hasql.TH as TH
import Hasql.Connection as Connection
import qualified Hasql.Session as Session
import qualified Data.Vector.Generic.Base as V

insertPerson :: V.Vector v Text => Statement (UUID, Text, Text, Day, Maybe (v Text)) ()
insertPerson =
  [TH.resultlessStatement|
    insert into people (id, apelido, nome, nascimento, stack)
    values ($1 :: uuid, $2 :: Text, $3 :: Text, $4 :: date, $5 :: Text[]?)
    |]

getPerson :: V.Vector v Text => Statement UUID (Maybe (UUID, Text, Text, Day, Maybe (v Text)))
getPerson =
  [TH.maybeStatement|
    select id :: uuid, apelido :: Text, nome :: Text, nascimento :: date, stack :: Text[]?
    from people
    where id = $1 :: uuid
    |]

searchPeople :: V.Vector v Text => Statement Text (Vector (UUID, Text, Text, Day, Maybe (v Text)))
searchPeople = 
  [TH.vectorStatement|
    select id :: uuid, apelido :: Text, nome :: Text, nascimento :: date, stack :: Text[]?
    from people
    where for_search like ('%' || $1 :: Text || '%')
    limit 50
    |]

countPeople :: Statement () Text
countPeople = [TH.singletonStatement| select count(1) :: Text from people |]

