module Model.SiteSpec
    ( main
    , spec
    ) where

import TestImport
import Model.Site

import qualified Web.Stripe.Plan as S

main :: IO ()
main = hspec spec

spec :: Spec
spec = withApp $ do
    describe "siteBranded" $ do
        it "returns True if the site is not associated with any users" $ do
            siteId <- runDB $ insert buildSite

            runDB (siteBranded siteId) `shouldReturn` True

        it "returns True if all users are on a non-commercial plan" $ do
            siteId <- runDB $ attachPlansToSite
                [ buildPlan { planName = S.PlanId "a", planCommercial = False }
                , buildPlan { planName = S.PlanId "b", planCommercial = False }
                , buildPlan { planName = S.PlanId "c", planCommercial = False }
                ]

            runDB (siteBranded siteId) `shouldReturn` True

        it "returns False if any users are on an commercial plan" $ do
            siteId <- runDB $ attachPlansToSite
                [ buildPlan { planName = S.PlanId "a", planCommercial = False }
                , buildPlan { planName = S.PlanId "b", planCommercial = True }
                , buildPlan { planName = S.PlanId "c", planCommercial = False }
                ]

            runDB (siteBranded siteId) `shouldReturn` False

attachPlansToSite :: [Plan] -> DB SiteId
attachPlansToSite plans = do
    siteId <- insert buildSite
    planIds <- forM plans insert
    userIds <- forM planIds $ \planId -> insert buildUser
        { userPlan = planId
        -- to make unique values
        , userIdent = pack $ show planId
        , userEmail = (pack $ show planId) ++ "@example.com"
        }

    void $ forM userIds $ \userId -> insert $ Membership siteId userId

    return siteId
