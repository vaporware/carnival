module IntegrationImport
    ( createDBPool
    , clickOn
    , fillIn
    , inLoginWindow
    , waitForElem
    , waitForText
    , module X
    ) where

import ClassyPrelude as X hiding (Element)
import Test.Hspec.WebDriver as X hiding (shouldBe, shouldReturn)

import Settings
import TestImport (getAppSettings, wipeDBPool)
import Test.WebDriver.Class
import Test.WebDriver.Commands.Wait

import Control.Monad.Logger
import Database.Persist.Postgresql

inLoginWindow :: WebDriver m => m a -> m ()
inLoginWindow f = do
    cw <- getCurrentWindow
    focusWindow =<< return . unsafeHead . filter (/= cw) =<< windows
    void $ f
    focusWindow cw

clickOn :: WebDriver m => Selector -> m ()
clickOn = click <=< waitForElem

fillIn :: WebDriver m => Selector -> Text -> m Element
fillIn x value = do
    input <- findElem x
    sendKeys value input
    return input

waitForText :: WebDriver m => Selector -> m Text
waitForText = getText <=< waitForElem

waitForElem :: WebDriver m => Selector -> m Element
waitForElem = waitUntil 50000 . findElem

createDBPool :: IO ConnectionPool
createDBPool = do
    settings <- getAppSettings
    pool <- runStderrLoggingT $ createPostgresqlPool
        (pgConnStr $ appDatabaseConf settings)
        (pgPoolSize $ appDatabaseConf settings)

    wipeDBPool pool

    return pool
