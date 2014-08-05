{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mappend)
import Hakyll

main :: IO ()
main = hakyll $ do
  -- Static resources
  --- Images
  match "images/*" $ do
    route idRoute
    compile copyFileCompiler
  --- Style sheets
  match "css/*" $ do
    route idRoute
    compile compressCssCompiler
  -- About page
  match "about.html" $ do
    route $ idRoute
    compile $ do
      getResourceBody >>= loadAndApplyTemplate "templates/default.html" defaultContext
                      >>= relativizeUrls
  -- Posts
  match "posts/*" $ do
    route $ setExtension "html"
    compile $ pandocCompiler >>= saveSnapshot "content"
                             >>= loadAndApplyTemplate "templates/post.html" postCtx
                             >>= loadAndApplyTemplate "templates/default.html" postCtx 
                             >>= relativizeUrls
  -- Archives
  create ["archives.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let archiveCtx = listField "posts" postCtx (return posts) `mappend`
                       constField "title" "Archives" `mappend`
                       defaultContext
      makeItem "" >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                  >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                  >>= relativizeUrls
  -- Index
  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"
      let indexCtx = listField "posts" teaserCtx (return posts) `mappend`
                     defaultContext
      getResourceBody >>= applyAsTemplate indexCtx
                      >>= loadAndApplyTemplate "templates/default.html" indexCtx
                      >>= relativizeUrls
  -- Templates
  match "templates/*" $ compile templateCompiler

-- Custom contexts
postCtx :: Context String
postCtx = dateField "date" "%F" `mappend`
          defaultContext

teaserCtx = teaserField "teaser" "content" `mappend` postCtx
