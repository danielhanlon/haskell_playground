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
  buildDb
  --dumpTable
  basic <- selectList [TutorialTitle ==. "Basic Haskell"] []
  liftIO $ print basic

buildDb = do
  runMigrationSilent migrateTables
  insert $ Tutorial "Basic Haskell" "https://fpcomplete.com/school/basic-haskell-1" True
  insert $ Tutorial "A monad tutorial" "https://fpcomplete.com/user/anne/monads" False
  insert $ Tutorial "Yesod usage" "https://fpcomplete.com/school/basic-yesod" True
  insert $ Tutorial "Putting the FUN in functors" "https://fpcomplete.com/user/anne/functors" False
  insert $ Tutorial "Basic Haskell" "https://fpcomplete/user/anne/basics" False

dumpTable = rawQuery "select * from Tutorial" [] $$ CL.mapM_ (liftIO . print)
