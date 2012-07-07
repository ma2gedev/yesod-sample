{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Todo where

import Import
import Yesod.Form.Jquery
import Data.Time
import Data.Text

getTodoCreateR :: Handler RepHtml
getTodoCreateR = do
    tags <- runDB $ selectList [] []
    let tagList = fmap (\tag -> (tagName $ entityVal tag, entityKey tag)) tags
    (formWidget, formEnctype) <- generateFormPost $ todoForm tagList Nothing
    defaultLayout $ do
        setTitle "TodoCreate"
        $(widgetFile "todoform")

postTodoCreateR :: Handler RepHtml
postTodoCreateR = do
    tags <- runDB $ selectList [] []
    let tagList = fmap (\tag -> (tagName $ entityVal tag, entityKey tag)) tags
    ((result, formWidget), formEnctype) <- runFormPost $ todoForm tagList Nothing
    case result of
        FormSuccess res -> do
            _ <- runDB $ insert res
            return ()
        _ -> return()
    defaultLayout $ do
        setTitle "TodoCreate"
        $(widgetFile "todoform")

getTodoUpdateR :: TodoId -> Handler RepHtml
getTodoUpdateR todoId = do
    tags <- runDB $ selectList [] []
    let tagList = fmap (\tag -> (tagName $ entityVal tag, entityKey tag)) tags
    todo <- runDB $ get404 todoId
    (formWidget, formEnctype) <- generateFormPost $ todoForm tagList (Just todo)
    defaultLayout $ do
        setTitle "TodoUpdate"
        $(widgetFile "todoform")

postTodoUpdateR :: TodoId -> Handler RepHtml
postTodoUpdateR todoId = do
    tags <- runDB $ selectList [] []
    let tagList = fmap (\tag -> (tagName $ entityVal tag, entityKey tag)) tags
    ((result, formWidget), formEnctype) <- runFormPost $ todoForm tagList Nothing
    case result of
        FormSuccess res -> do
            _ <- runDB $ update todoId
                [ TodoTitle =. todoTitle res
                , TodoDescription =. todoDescription res
                , TodoTag =. todoTag res
                , TodoDone =. todoDone res
                , TodoDeleted =. todoDeleted res
                , TodoCreated =. todoCreated res
                ]
            return ()
        _ -> return()
    defaultLayout $ do
        setTitle "TodoUpdate"
        $(widgetFile "todoform")

todoForm :: [(Text, TagId)] -> Maybe Todo -> Form Todo
todoForm tagList mtodo = renderDivs $ Todo
    <$> areq textField "TITLE" (fmap todoTitle mtodo)
    <*> areq textField "DESCRIPTION" (fmap todoDescription mtodo)
    <*> areq (selectFieldList tagList) "TAG" (fmap todoTag mtodo)
    <*> areq (selectFieldList [(pack "done", True), (pack "undone", False)]) "DONE" (fmap todoDone mtodo)
    <*> areq (selectFieldList [(pack "exist", False), (pack "deleted", True)]) "DELETED" (fmap todoDeleted mtodo)
    <*> areq (jqueryDayField def { jdsChangeYear = True, jdsYearRange = "2000:+5"}) "CREATED" (fmap todoCreated mtodo)

getTodosR :: Handler RepHtml
getTodosR = do
    todoEntities <- runDB $ selectList [] [Desc TodoTitle]
    tagEntities <- runDB $ selectList [] [Desc TagName]
    defaultLayout $ do
        setTitle "Todos"
        $(widgetFile "todos")

lookUpTag todoEntity tagEntity = entityKey tagEntity == todoTag (entityVal todoEntity)
