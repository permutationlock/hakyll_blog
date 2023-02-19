--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mappend)
import Data.Maybe
import Data.List
import Text.Pandoc.Definition
import Text.Pandoc.Highlighting (Style, breezeDark, styleToCss)
import Text.Pandoc.Options (ReaderOptions (..), WriterOptions (..))
import Hakyll
import Hakyll.Web.Pandoc
    ( defaultHakyllReaderOptions
    , defaultHakyllWriterOptions
    )

--------------------------------------------------------------------------------
main :: IO ()
main = do
    hakyll $ do
        match "css/*" $ do
            route   idRoute
            compile compressCssCompiler

        -- build up tags
        tags <- buildTags "posts/*" (fromCapture "tags/*.html")

        match "posts/*.md" $ do
            route $ setExtension "html"
            compile $ do
                tagsList <- (getUnderlying >>= getTags)
                let ctx = (postCtxWithTagList tagsList)
                pandocCompiler'
                    >>= loadAndApplyTemplate "templates/post.html" ctx
                    >>= loadAndApplyTemplate "templates/default.html" ctx
                    >>= relativizeUrls

        tagsRules tags $ \tag pattern -> do
            let title = "Posts related to " ++ tag
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll pattern
                let ids = map itemIdentifier posts
                tagsList <- nub . concat <$> traverse getTags ids
                let ctx = constField "title" title
                          `mappend` postCtxWithTagList tagsList
                          `mappend` constField "page-posts" ""
                          `mappend` listField "posts" (postCtxWithTags tags) (return posts)
                          `mappend` defaultContext

                makeItem ""
                    >>= loadAndApplyTemplate "templates/posts.html" ctx
                    >>= loadAndApplyTemplate "templates/default.html" ctx
                    >>= relativizeUrls

        match "pages/*.md" $ do
            route $ setExtension "html"
            compile $ do
                pandocCompiler'
                    >>= loadAndApplyTemplate "templates/post.html" postCtx
                    >>= loadAndApplyTemplate "templates/default.html" postCtx
                    >>= relativizeUrls

        create ["index.html"] $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll "posts/*"
                let ids = map itemIdentifier posts
                tagsList <- nub . concat <$> traverse getTags ids
                let blogCtx =
                        constField "title" "Posts" `mappend`
                        listField "posts" postCtx (return posts) `mappend`
                        postCtxWithTagList tagsList `mappend`
                        constField "page-posts" "" `mappend`
                        constField "untagged" "" `mappend`
                        defaultContext

                makeItem ""
                    >>= loadAndApplyTemplate "templates/posts.html" blogCtx
                    >>= loadAndApplyTemplate "templates/default.html" blogCtx
                    >>= relativizeUrls

        --create ["css/syntax.css"] $ do
        --    route idRoute
        --    compile $ do
        --        makeItem $ styleToCss pandocCodeStyle

        match "quotes.md" $ do
            route $ setExtension "html"
            compile $ do 
                let cvCtx =
                        constField "page-quotes" "" `mappend`
                        defaultContext
                pandocCompiler'
                    >>= loadAndApplyTemplate "templates/default.html" cvCtx
                    >>= relativizeUrls

        match "about.md" $ do
            route $ setExtension "html"
            compile $ do
                let aboutCtx =
                        constField "page-about" "" `mappend`
                        defaultContext
                pandocCompiler'
                    >>= loadAndApplyTemplate "templates/default.html" aboutCtx
                    >>= relativizeUrls

        match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
pandocCodeStyle :: Style
pandocCodeStyle = breezeDark

pandocCompiler' :: Compiler (Item String)
pandocCompiler' =
  pandocCompilerWith
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions
      { writerHighlightStyle   = Just pandocCodeStyle
      }

youtubeCtx :: Context a
youtubeCtx = field "youtube" $ \item -> do
    metadata <- getMetadata (itemIdentifier item)
    return $ fromMaybe "" $ lookupString "youtube" metadata

gitCtx ::  Context String
gitCtx = field "git" $ \item -> do
    metadata <- getMetadata (itemIdentifier item)
    return $ fromMaybe "" $ lookupString "git" metadata


postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    metadataField `mappend`
    defaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags `mappend` postCtx


postCtxWithTagList :: [String] -> Context String
postCtxWithTagList tags = listField "tagsList" (field "tag" $ pure . itemBody) (traverse makeItem tags)
    <> postCtx
