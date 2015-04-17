module Main where

import TestImport
import Test.Hspec.WebDriver hiding (shouldReturn)
import Test.WebDriver.Commands.Wait

import Control.Monad.Logger
import Database.Persist.Postgresql
import Database.Persist.Sql

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    user <- runIO $ do
        pool <- createDBPool
        runDBWithPool pool $ createUser "1"

    describe "" $ session "" $ using Firefox $ do
        it "loads carnival" $ runWD $ do
            openPage "http://localhost:3000"
            click =<< waitForElem (ByCSS ".carnival-comment-indicator")
            click =<< waitForElem (ByCSS ".carnival-create")

            inLoginWindow $
                submit =<< fillIn (ByName "ident") (userIdent $ entityVal user)

            submit =<< fillIn (ByCSS ".carnival-body") "a comment"

            openPage "http://localhost:3000"
            click =<< waitForElem (ByCSS ".carnival-comment-indicator")
            click =<< waitForElem (ByCSS ".carnival-create")

            (getText =<< waitForElem (ByCSS ".carnival-comment .carnival-body"))
                `shouldReturn` "a comment"

inLoginWindow f = do
    cw <- getCurrentWindow
    focusWindow =<< return . unsafeHead . filter (/= cw) =<< windows
    f
    focusWindow cw

login ident = do
    openPage "http://localhost:3000/auth/login"
    submit =<< fillIn (ByName "ident") "1"

fillIn x value = do
    input <- findElem x
    sendKeys value input
    return input

waitForElem = waitUntil 50000 . findElem

createDBPool :: IO ConnectionPool
createDBPool = do
    settings <- getAppSettings
    pool <- runStderrLoggingT $ createPostgresqlPool
        (pgConnStr $ appDatabaseConf settings)
        (pgPoolSize $ appDatabaseConf settings)

    wipeDBPool pool

    return pool
