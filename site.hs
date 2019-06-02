--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mappend)
import Data.Maybe
import Text.Pandoc.Definition
import Hakyll
import Hakyll.Web.Pandoc (defaultHakyllReaderOptions, defaultHakyllWriterOptions, pandocCompilerWithTransformM)
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
        
        match "images/*" $ do
            route   idRoute
            compile copyFileCompiler

        match "css/*" $ do
            route   idRoute
            compile compressCssCompiler

        match (fromList ["about.rst", "contact.markdown"]) $ do
            route   $ setExtension "html"
            compile $ pandocCompiler
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls


        match "posts/*" $ do
            route $ setExtension "html"
            compile $ postCompiler

        create ["archive.html"] $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll "posts/*"
                let archiveCtx =
                        listField "posts" postCtx (return posts) `mappend`
                        constField "title" "Archives"            `mappend`
                        defaultContext

                makeItem ""
                    >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                    >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                    >>= relativizeUrls


        match "index.html" $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll "posts/*"
                let indexCtx =
                        listField "posts" postCtx (return posts) `mappend`
                        constField "title" "Home"                `mappend`
                        defaultContext

                getResourceBody
                    >>= applyAsTemplate indexCtx
                    >>= loadAndApplyTemplate "templates/default.html" indexCtx
                    >>= relativizeUrls

        match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

postCompiler :: Compiler (Item String)
postCompiler = do
    metadataPrelude <- getUnderlying >>= flip getMetadataField "header-includes" 
    pandocCompilerWithTransformM defaultHakyllReaderOptions defaultHakyllWriterOptions
        $ compileFormulaeDataURI defaultEnv (fo metadataPrelude)
    where
        fo hi = case hi of
            Nothing -> defaultPandocFormulaOptions
            Just s -> PandocFormulaOptions {
                    shrinkBy = 2,
                    errorDisplay = displayError,
                    formulaOptions = \mathType -> case mathType of
                        DisplayMath -> FormulaOptions s "displaymath" 200;
                        _ -> FormulaOptions s "math" 200
                }
