{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Tag where

import Import
import Control.Monad (when)

getTagCreateR :: Handler RepHtml
getTagCreateR = do
    (formWidget, formEnctype) <- generateFormPost (tagForm Nothing)
    defaultLayout $ do
        setTitle "TagCreate"
        $(widgetFile "tagform")

postTagCreateR :: Handler RepHtml
postTagCreateR = do
    ((result, formWidget), formEnctype) <- runFormPost (tagForm Nothing)
    case result of
        FormSuccess res -> do
            _ <- runDB $ insert res
            return ()
        _ -> do
            return ()
    defaultLayout $ do
        setTitle "TagCreate"
        $(widgetFile "tagform")

tagForm :: Maybe Tag -> Form Tag
tagForm mtag = renderDivs $ Tag
    <$> areq textField "Name" (fmap tagName mtag)

getTagUpdateR :: TagId -> Handler RepHtml
getTagUpdateR tagid = do
    tag <- runDB $ get404 tagid
    (formWidget, formEnctype) <- generateFormPost (tagForm $ Just tag)
    defaultLayout $ do
        setTitle "TagUpdate"
        $(widgetFile "tagform")

postTagUpdateR :: TagId -> Handler RepHtml
postTagUpdateR tagid = do
    ((result, formWidget), formEnctype) <- runFormPost (tagForm Nothing)
    case result of
        FormSuccess res -> do
            _ <- runDB $ update tagid [TagName =. tagName res]
            return ()
        _ -> do
            return ()
    defaultLayout $ do
        setTitle "TagUpdate"
        $(widgetFile "tagform")

getTagsR :: Int -> Handler RepHtml
getTagsR page = do
    when (page <= 0) notFound
    let perPage = 4
        sNum = (page - 1) * perPage + 1
        eNum = page * perPage
    tagEntities <- runDB $ selectList [] [OffsetBy (sNum - 1), LimitTo perPage]
    tagCount <- runDB $ count ([] :: [Filter Tag])
    let maxPage = (tagCount + perPage - 1) `div` perPage
    defaultLayout $ do
        setTitle "Tags"
        $(widgetFile "tags")

