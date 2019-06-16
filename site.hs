--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mappend)
import Data.Maybe
import Text.Pandoc.Definition
import Hakyll
import Hakyll.Web.Pandoc
    ( defaultHakyllReaderOptions
    , defaultHakyllWriterOptions
    , pandocCompilerWithTransformM
    )
import Image.LaTeX.Render
import Image.LaTeX.Render.Pandoc
import Hakyll.Contrib.LaTeX
    ( initFormulaCompilerDataURI
    , CacheSize
    , compileFormulaeDataURI
    )

--------------------------------------------------------------------------------
main :: IO ()
main = do
    renderFormulae <- initFormulaCompilerDataURI 1000 defaultEnv
    hakyll $ do
        match "css/*" $ do
            route   idRoute
            compile compressCssCompiler

        match "posts/*.md" $ do
            route $ setExtension "html"
            compile $ do
                maybePreamble <- (getUnderlying
                    >>= flip getMetadataField "header-includes")
                pandocCompilerWithTransformM defaultHakyllReaderOptions
                    defaultHakyllWriterOptions
                    (renderFormulae $ formulaOptionsFromPreamble maybePreamble)
                    >>= loadAndApplyTemplate "templates/post.html" postCtx
                    >>= loadAndApplyTemplate "templates/default.html" postCtx
                    >>= relativizeUrls

        match "notes/*.md" $ do
            route $ setExtension "html"
            compile $ do
                maybePreamble <- (getUnderlying
                    >>= flip getMetadataField "header-includes")
                pandocCompilerWithTransformM defaultHakyllReaderOptions
                    defaultHakyllWriterOptions
                    (renderFormulae $ formulaOptionsFromPreamble maybePreamble)
                    >>= loadAndApplyTemplate "templates/post.html" defaultContext
                    >>= loadAndApplyTemplate "templates/default.html" defaultContext
                    >>= relativizeUrls

        create ["posts.html"] $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll "posts/*"
                let blogCtx =
                        listField "posts" postCtx (return posts) `mappend`
                        constField "title" "Posts"            `mappend`
                        constField "page-posts" "" `mappend`
                        defaultContext

                makeItem ""
                    >>= loadAndApplyTemplate "templates/posts.html" blogCtx
                    >>= loadAndApplyTemplate "templates/default.html" blogCtx
                    >>= relativizeUrls

        create ["notes.html"] $ do
            route idRoute
            compile $ do
                posts <- loadAll "notes/*"
                let blogCtx =
                        listField "notes" defaultContext (return posts) `mappend`
                        constField "title" "Notes"            `mappend`
                        constField "page-notes" "" `mappend`
                        defaultContext

                makeItem ""
                    >>= loadAndApplyTemplate "templates/notes.html" blogCtx
                    >>= loadAndApplyTemplate "templates/default.html" blogCtx
                    >>= relativizeUrls

        match "cv.md" $ do
            route $ setExtension "html"
            compile $ do 
                let cvCtx =
                        constField "page-cv" "" `mappend`
                        defaultContext
                maybePreamble <- (getUnderlying
                    >>= flip getMetadataField "header-includes")
                pandocCompilerWithTransformM defaultHakyllReaderOptions
                    defaultHakyllWriterOptions
                    (renderFormulae $ formulaOptionsFromPreamble maybePreamble)
                    >>= loadAndApplyTemplate "templates/default.html" cvCtx
                    >>= relativizeUrls

        match "index.md" $ do
            route $ setExtension "html"
            compile $ do
                let aboutCtx =
                        constField "page-about" "" `mappend`
                        defaultContext
                maybePreamble <- (getUnderlying
                    >>= flip getMetadataField "header-includes")
                pandocCompilerWithTransformM defaultHakyllReaderOptions
                    defaultHakyllWriterOptions
                    (renderFormulae $ formulaOptionsFromPreamble maybePreamble)
                    >>= loadAndApplyTemplate "templates/default.html" aboutCtx
                    >>= relativizeUrls

        match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

formulaOptionsFromPreamble :: Maybe String -> PandocFormulaOptions
formulaOptionsFromPreamble Nothing = defaultPandocFormulaOptions
formulaOptionsFromPreamble (Just pre) = defaultPandocFormulaOptions {
        formulaOptions = \mathtype -> case mathtype of
                DisplayMath -> displaymath { preamble = pre }
                _           -> math        { preamble = pre }
    }
