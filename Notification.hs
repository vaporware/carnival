module Notification
    ( Notification(..)
    , Recipient(..)
    , sendNotification

    -- Exported for testing
    , notificationName
    , notificationRecipients
    ) where

import Import
import Model.User
import Model.UserComment
import Model.Subscription
import SendMail

import Data.Text.Lazy.Builder (toLazyText)
import Network.Mail.Mime (simpleMail')

data Notification = NewComment UserComment

data Recipient = Recipient
    { recipientUser :: User
    , recipientToken :: Token
    }
    deriving (Eq, Show)

notificationUser :: Notification -> User
notificationUser (NewComment userComment) =
    entityVal $ userCommentUser userComment

notificationSite :: Notification -> Site
notificationSite (NewComment userComment) =
    entityVal $ userCommentSite userComment

notificationComment :: Notification -> Comment
notificationComment (NewComment userComment) =
    entityVal $ userCommentComment userComment

recipientAddress :: Recipient -> Address
recipientAddress r =
    let u = recipientUser r
    in Address (Just $ userName u) $ userEmail u

sendNotification :: Notification -> Handler ()
sendNotification n = do
    recipients <- runDB $ notificationRecipients n

    mapM_ (sendMail <=< notificationToMail n) recipients

notificationName :: Notification -> Text
notificationName n =
    let c = notificationComment n
    in "new-comment-" <> commentArticleURL c <> "-" <> commentThread c

notificationFrom :: Notification -> Address
notificationFrom n =
    let u = notificationUser n
    in Address (Just $ userName u) "carnival-notifications@thoughtbot.com"

notificationRecipients :: Notification -> DB [Recipient]
notificationRecipients n = do
    let c = notificationComment n

    subs <- activeSubscriptions' $ notificationName n
    users <- findUsers $ filter (/= commentUser c) $ map subscriptionUser subs

    fmap catMaybes $ forM users $ \(Entity uid u) -> do
        let msub = find ((== uid) . subscriptionUser) subs

        return $ fmap (Recipient u . subscriptionToken) msub

notificationToMail :: Notification -> Recipient -> Handler Mail
notificationToMail n r = do
    let c = notificationComment n
        subject = "New comment on " <> commentArticleTitle c
        comment = unMarkdown $ commentBody c
        baseUrl = siteBaseUrl $ notificationSite n
        articleUrl = commentArticleURL c
        unsubscribeRoute = UnsubscribeR $ recipientToken r

    body <- toLazyText <$> withUrlRenderer $(textFile "mail/new_comment")

    return $ simpleMail' (recipientAddress r) (notificationFrom n) subject body
