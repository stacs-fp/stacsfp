--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative
import Control.Monad (unless)
import Data.Foldable (for_)
import qualified Data.List as List
import Data.Maybe
import Data.Monoid (mappend)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Traversable (for)
import Hakyll
import Hakyll.Core.Identifier (fromFilePath)
import qualified Text.BibTeX.Entry as BibEntry
import qualified Text.BibTeX.Format as BibFormat
import Text.BibTeX.Parse (file, skippingLeadingSpace )
import Text.Parsec.String (parseFromFile)
import System.Directory (createDirectoryIfMissing, doesFileExist, getDirectoryContents)
import System.FilePath ((</>), (<.>), takeBaseName, takeExtension)
import System.IO (hPutStrLn, stderr)

bibEntryFilePath :: BibEntry.T -> FilePath
bibEntryFilePath e = "bibtex" </> BibEntry.identifier e <.> "bib"

writeBibs :: [BibEntry.T] -> IO ()
writeBibs =
  mapM_ $ \b -> do
    p <- doesFileExist (bibEntryFilePath b)
    unless p (writeFile (bibEntryFilePath b) (BibFormat.entry b))

splitBibs :: IO [BibEntry.T]
splitBibs = concat <$> do
  fs <- filter ((== ".bib") . takeExtension) <$> getDirectoryContents "publications"
  createDirectoryIfMissing True "bibtex"
  for fs $ \f -> do
    r <- parseFromFile (skippingLeadingSpace file) ("publications" </> f)
    case r of
      Left err -> do
        hPutStrLn stderr ("Error processing file '" ++ f ++ "' (skipping): " ++ show err)
        return []
      Right bs -> do
        writeBibs bs
        return bs

bibIdMap :: [BibEntry.T] -> Map String BibEntry.T
bibIdMap = foldr (\b -> Map.insert (BibEntry.identifier b) b) Map.empty

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    bibs <- preprocess $ bibIdMap <$> splitBibs

    match ("js/*" .||.
           "lib/**/*" .||.
           "images/**" .||.
           "bibtex/*") $ do
      route idRoute
      compile copyFileCompiler

    match "css/*" $ do
      route   idRoute
      compile compressCssCompiler

    match "people/*" $
      compile $ pandocCompiler >>= applyAsTemplate standardContext

    match "posts/*" $ do
      let ctx = constField "active" "news" `mappend` standardContext
      route $ setExtension "html"
      compile $ pandocCompiler >>=
        loadAndApplyTemplate "templates/post.html" ctx >>=
        saveSnapshot "content" >>=
        loadAndApplyTemplate "templates/default.html" ctx >>=
        relativizeUrls

    match "pages/*" $ do
      let ctx = standardContext -- `mappend` publicationContext bibs
      let compiler id = if ".html" == takeExtension (toFilePath id)
                           then getResourceBody
                           else pandocCompiler
      route $ gsubRoute "pages/" (const "") `composeRoutes`
              setExtension "html"
      compile $ getUnderlying >>= compiler >>=
        applyAsTemplate ctx >>=
        loadAndApplyTemplate "templates/default.html" ctx >>=
        relativizeUrls

    match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------
dateContext = dateField "date" "%B %e, %Y"

standardContext :: Context String
standardContext = mconcat
  [overrideTitle "title"
  ,titleField "active"
  ,newsPostsField
  ,postsField
  ,peopleContext
  ,dateContext
  ,defaultContext]

thumbContext :: Context String
thumbContext = field "thumb" $ \item ->
  return . fromMaybe "recent-post.png" .
                     Map.lookup "thumb" =<< getMetadata (itemIdentifier item)

newsPostsField = withPostsField "newsposts" recentFirst
postsField = withPostsField "posts" (fmap (take 2) . recentFirst)

withPostsField fld c =
  listField
    fld
    (mconcat [dateField "day" "%d", dateField "shortmonth" "%b"
             ,teaserField "teaser" "content"
             ,dateContext, thumbContext, defaultContext])
    (c =<< loadAllSnapshots "posts/*" "content")

peopleContext =
  listField "people"
    (avatar `mappend` defaultContext)
    (loadAll "people/*" :: Compiler [Item String])
  where avatar = field "avatar" $ \item -> do
          let itemId = itemIdentifier item
          let defaultAvatar = takeBaseName (toFilePath itemId)
          metadata <- getMetadata itemId
          return $ fromMaybe defaultAvatar (Map.lookup "avatar" metadata)

overrideTitle :: String -> Context String
overrideTitle fld = do
  field fld $ \item -> do
   let itemId = itemIdentifier item
   let defaultTitle = takeBaseName (toFilePath itemId)
   title <- getMetadata itemId
   return $ fromMaybe defaultTitle (Map.lookup "title" title)

--paperContext bibs = field "year" $ \item -> do
--  let Just bib = Map.lookup (show $ itemIdentifier item) bibs
--  return . fromJust . List.lookup "year" $ BibEntry.fields bib
--
--publicationContext bibs =
--  listField
--    "publications"
--    (paperContext bibs `mappend` missingField)
--    (fmap sequence . makeItem . fromJust $ Map.lookup 2012 bibs)
