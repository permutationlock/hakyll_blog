--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mappend)
import Data.Maybe
import Data.Map
import Data.List
import Data.Either
import Skylighting ( SyntaxMap, defaultSyntaxMap )
import Skylighting.Loader ( loadSyntaxesFromDir )
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
    syntaxOrErr <- loadSyntaxesFromDir "syntax"
    let customSyntaxMap = fromRight Data.Map.empty syntaxOrErr
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
                pandocCompiler' customSyntaxMap
                    >>= loadAndApplyTemplate "templates/post.html" ctx
                    >>= loadAndApplyTemplate "templates/default.html" ctx
                    >>= relativizeUrls

        tagsRules tags $ \tag pattern -> do
            let title = "Posts related to " ++ tag
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll pattern
                let ids = Data.List.map itemIdentifier posts
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
                pandocCompiler' customSyntaxMap
                    >>= loadAndApplyTemplate "templates/post.html" postCtx
                    >>= loadAndApplyTemplate "templates/default.html" postCtx
                    >>= relativizeUrls

        create ["index.html"] $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll "posts/*"
                let ids = Data.List.map itemIdentifier posts
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
                pandocCompiler' customSyntaxMap
                    >>= loadAndApplyTemplate "templates/default.html" cvCtx
                    >>= relativizeUrls

        match "about.md" $ do
            route $ setExtension "html"
            compile $ do
                let aboutCtx =
                        constField "page-about" "" `mappend`
                        defaultContext
                pandocCompiler' customSyntaxMap
                    >>= loadAndApplyTemplate "templates/default.html" aboutCtx
                    >>= relativizeUrls

        match "templates/*" $ compile templateBodyCompiler
        match "images/*" $ do
            route   idRoute
            compile copyFileCompiler


--------------------------------------------------------------------------------
pandocCodeStyle :: Style
pandocCodeStyle = breezeDark

pandocCompiler' :: SyntaxMap -> Compiler (Item String)
pandocCompiler' customSyntaxMap =
  pandocCompilerWith
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions
      { writerHighlightStyle   = Just pandocCodeStyle,
        writerSyntaxMap = (Data.Map.union customSyntaxMap defaultSyntaxMap)
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
