{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Tag where

import Import

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

getTagsR :: Handler RepHtml
getTagsR = do
    tagEntities <- runDB $ selectList [] []
    defaultLayout $ do
        setTitle "Tags"
        $(widgetFile "tags")

