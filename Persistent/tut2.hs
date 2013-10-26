{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}

import Data.Text (Text)
import Database.Persist

import Database.Persist.Sqlite (runSqlite, runMigrationSilent)
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase, share, sqlSettings)

import Database.Persist.Sql (rawQuery)
import Data.Conduit (($$))
import Data.Conduit.List as CL
import Control.Monad.IO.Class (liftIO)

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
Tutorial
  title     Text
  url       Text
  school    Bool
  deriving  Show
|]

main = runSqlite ":memory:" $ do
  runMigrationSilent migrateTables
  dumpTable

dumpTable = rawQuery "select * from Tutorial" [] $$ CL.mapM_ (liftIO . print)
