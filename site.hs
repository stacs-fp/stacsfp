--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Data.Maybe
import Data.Monoid (mappend)
import qualified Data.Map as M
import Hakyll
import System.FilePath (takeBaseName)

copyAll = route idRoute >> compile copyFileCompiler
copyAllIn path = match path copyAll

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    -- FIXME there's surely a better way of doing this...
    mapM_ copyAllIn
      ["js/*"
      ,"lib/**/*"
      ,"images/*"
      ,"images/**/*"]

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    --match (fromList ["about.rst", "contact.markdown"]) $ do
    --    route   $ setExtension "html"
    --    compile $ pandocCompiler
    --        >>= loadAndApplyTemplate "templates/default.html" defaultContext
    --        >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $
            pandocCompiler
              >>= loadAndApplyTemplate "templates/post.html"    standardContext
              >>= loadAndApplyTemplate "templates/default.html" standardContext
              >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $
            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" standardContext
                >>= loadAndApplyTemplate "templates/default.html" standardContext
                >>= relativizeUrls


    match "*.html" $ do
        route idRoute
        compile $ do
            getResourceBody
                >>= applyAsTemplate standardContext
                >>= loadAndApplyTemplate "templates/default.html" standardContext
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------
standardContext :: Context String
standardContext =
  overrideTitle "title" `mappend`
  titleField "active" `mappend`
  dateField "date" "%B %e, %Y" `mappend`
  postsField `mappend`
  defaultContext

postsField :: Context String
postsField =
  listField
    "posts"
    (mconcat [field "url" $ return . const "url0"
             ,field "title" $ return . const "title0"
             ,field "date" $ return . const "date0"])
    (fmap sequence (makeItem ["A", "B"]))
  --  --(recentFirst =<< loadAll ("posts/*" .&&. complement )
  --  --   :: Compiler [Item String])

overrideTitle :: String -> Context String
overrideTitle fld = do
  field fld $ \item -> do
   let itemId = itemIdentifier item
   let defaultTitle = takeBaseName (toFilePath itemId)
   title <- getMetadata itemId
   return $ fromMaybe defaultTitle (M.lookup "title" title)
