--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings  #-}
module Main (main) where


--------------------------------------------------------------------------------
import           Data.List       (isSuffixOf)
import           Prelude         hiding (id)
import           System.FilePath

--------------------------------------------------------------------------------
import           Hakyll


hakyllConf :: Configuration
hakyllConf = defaultConfiguration {
    providerDirectory = "provider"
  , destinationDirectory = "_site"
  , storeDirectory = "_temp/cache"
  , tmpDirectory = "_temp/cache/tmp"
  }

main :: IO ()
main = hakyllWith hakyllConf site

site :: Rules ()
site = do
    match "images/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "img/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "js/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "fonts/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/**" $ do
        route   idRoute
        compile compressCssCompiler


    match (fromList ["our-story.html", "when-and-where.html", "lodging.html", "registry.html", "photos.html", "social.html"]) $ do
        route   $ cleanRoute
        compile $ getResourceBody
            >>= loadAndApplyTemplate "templates/default.html" defaultCtx
            >>= relativizeUrls
            >>= cleanIndexUrls

    match "index.html" $ do
        route   $ idRoute
        compile $ do
            let indexCtx =
                    constField "title" "Home"                `mappend`
                    defaultCtx
            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls
                >>= cleanIndexHtmls


    match "templates/*" $ compile templateCompiler


defaultCtx :: Context String
defaultCtx = mconcat
  [ niceUrlField  "url"
  , defaultContext ]


--------------------------------------------------------------------------------
-- url field without trailing /index.html
niceUrlField :: String -> Context a
niceUrlField key = field key niceItemUrl

niceItemUrl :: Item a -> Compiler String
niceItemUrl =
  fmap (maybe "" (removeIndexStr . toUrl)) . getRoute . setVersion Nothing . itemIdentifier
  where removeIndexStr url = case splitFileName url of
            (dir, "index.html") -> dir
            _ -> url


--------------------------------------------------------------------------------
-- | custom routes
cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute
  where
    createIndexRoute ident = takeDirectory p </> takeBaseName p </> "index.html"
                            where p = toFilePath ident

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanIndex)

cleanIndexHtmls :: Item String -> Compiler (Item String)
cleanIndexHtmls = return . fmap (replaceAll pattern replacement)
    where
      pattern = "/index.html"
      replacement = const "/"

cleanIndex :: String -> String
cleanIndex url
    | idx `isSuffixOf` url = take (length url - length idx) url
    | otherwise            = url
  where idx = "index.html"

