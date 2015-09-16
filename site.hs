--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative
import Control.Monad (guard, unless)
import Data.Foldable (for_)
import qualified Data.List as List
import Data.List.Split (splitOn)
import Data.Maybe
import Data.Monoid ((<>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Traversable (for)
import Hakyll
import Hakyll.Core.Identifier (fromFilePath)
import qualified Text.BibTeX.Entry as BibEntry
import qualified Text.BibTeX.Format as BibFormat
import Text.BibTeX.Parse (file, skippingLeadingSpace)
import Text.LaTeX.Character (toUnicodeString)
import Text.Parsec.String (parseFromFile)
import System.Directory (createDirectoryIfMissing, doesFileExist, getDirectoryContents)
import System.FilePath.Posix (takeDirectory)
import System.FilePath ((</>), (<.>), takeBaseName, takeExtension)
import System.IO (hPutStrLn, stderr)

bibEntryFilePath :: BibEntry.T -> FilePath
bibEntryFilePath e =
  "bibtex" </> BibEntry.identifier e <.> "bib"

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

bibYearMap :: [BibEntry.T] -> Map String [BibEntry.T]
bibYearMap = foldr (\b m -> maybe m (\yr -> Map.insertWith (++) yr [b] m)
                                  (lookup "year" (BibEntry.fields b)))
                   Map.empty

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    bibs <- preprocess $ bibYearMap <$> splitBibs

    match ("js/*" .||.
           "lib/**/*" .||.
           "images/**" .||.
           "bibtex/**") $ do
      route idRoute
      compile copyFileCompiler

    match "css/*" $ do
      route   idRoute
      compile compressCssCompiler

    tags <- buildTags "posts/**" (fromCapture "tags/*.html")
    createTagsPages tags

    categories <- buildCategories "posts/**" (fromCapture "categories/*.html")
    createTagsPages categories

    match "people/*" $
      compile $ pandocCompiler >>= applyAsTemplate standardContext

    match "posts/**" $ do
      let ctx = constField "active" "news"
             <> tagsCloudField "tags" tags
             <> categoriesField "categories" categories
             <> standardContext
      route $ setExtension "html"
      compile $ pandocCompiler >>=
        saveSnapshot "content" >>=
        loadAndApplyTemplate "templates/post.html" ctx >>=
        applyAsTemplate ctx >>=
        saveSnapshot "formatted" >>=
        loadAndApplyTemplate "templates/default.html" ctx >>=
        relativizeUrls

    match "pages/*" $ do
      let ctx = publicationContext bibs
             <> tagsCloudField "tags" tags
             <> categoriesField "categories" categories
             <> withPostsField "newsposts" recentFirst
             <> standardContext
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

createTagsPages tags =
 tagsRules tags $ \tag pat -> do
   let ctx = constField "tag" tag
          <> constField "active" "news"
          <> constField "title" ("Posts tagged " ++ tag)
          <> listField "newsposts"
               postsFieldContext
               (recentFirst =<< loadAll pat)
          <> baseContext
   route idRoute
   compile $ makeItem "" >>=
     applyAsTemplate ctx >>=
     loadAndApplyTemplate "templates/tag.html" ctx >>=
     loadAndApplyTemplate "templates/default.html"
       (bodyField "body" <> ctx) >>=
     relativizeUrls

--------------------------------------------------------------------------------
dateContext = dateField "date" "%B %e, %Y"

tagsCloudField :: String -> Tags -> Context String
tagsCloudField fld tags = field fld . const $ renderTagCloud 90.0 250.0 tags

baseContext :: Context String
baseContext = mconcat
  [overrideTitle "title"
  ,titleField "active"
  ,withPostsField "posts" (fmap (take 2) . recentFirst)
  ,peopleContext
  ,dateContext]

standardContext :: Context String
standardContext = baseContext <> defaultContext

thumbContext :: Context String
thumbContext = field "thumb" $ \item ->
  return . fromMaybe "recent-post.png" .
                     Map.lookup "thumb" =<< getMetadata (itemIdentifier item)

postDateContext = dateField "day" "%d" <>
                  dateField "shortmonth" "%b"
postsFieldContext = mconcat
  [postDateContext
  ,dateContext
  ,thumbContext
  ,teaserField "teaser" "content"
  ,defaultContext]

withPostsField fld c =
  listField fld postsFieldContext (c =<< loadAllSnapshots "posts/**" "content")

peopleContext =
  listField "people"
    (avatar <> defaultContext)
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

paperContext = mconcat
  [listFieldWith "authors"
     (field "authorfirst" (return . wrapFirstLetter . trimSpace . drop 1 . snd . itemBody) <>
      field "authorlast" (return . trimSpace . fst . itemBody))
     (fmap sequence . makeItem . maybe [] (parseAuthors . toUnicodeString) . lookupField "author")
  ,mkfield "title"
  ,mkfield "booktitle"
  ,mkfield "address"
  ,field "editorlast" (getField "editor")
  ,field "editorfirst" (getField "editor")
  ,mkfield "volume"
  ,mkfield "series"
  ,mkfield "edition"
  ,mkfield "organization"
  ,mkfield "publisher"
  ,mkfield "pages"
  ,mkfield "howpublished"
  ,mkfield "school"
  ,mkfield "year"
  ,mkfield "note"
  ,field "bibtex" (return . bibEntryFilePath . itemBody)]
  where trimSpace = dropWhile (== ' ')

        wrapFirstLetter :: String -> String
        wrapFirstLetter []     = []
        wrapFirstLetter (x:xs) = "<span class=\"head\">" ++ x : "</span>" ++
                                 "<span class=\"tail\">" ++ xs ++ "</span>"

        lookupField :: String -> Item BibEntry.T -> Maybe String
        lookupField fld = lookup fld . BibEntry.fields . itemBody

        getField :: String -> Item BibEntry.T -> Compiler String
        getField fld = maybe empty (return . toUnicodeString) . lookupField fld

        mkfield :: String -> Context BibEntry.T
        mkfield fld = field fld (getField fld)

        parseAuthors :: String -> [(String, String)]
        parseAuthors = map (span (/= ',')) . splitOn "and"

papersContext bibs =
  listFieldWith
    "publications"
    paperContext
    (fmap sequence . makeItem . snd . itemBody)

publicationContext bibs =
  listField
    "publicationyears"
    (field "year" (return . fst . itemBody) <>
     papersContext bibs <>
     missingField)
    (fmap sequence . makeItem . reverse $ Map.toList bibs)

categoriesField :: String -> Tags -> Context a
categoriesField fld tags =
  listField fld
    (field "url" (return . snd . itemBody) <>
     field "category" (return . fst . itemBody))
    (do ts <- for (tagsMap tags) $ \(tag, _) -> do
          r <- getRoute (tagsMakeId tags tag)
          return (tag, r)
        fmap sequence . makeItem .
          map (\(x, y) -> (x, fromJust y)) . filter (isJust . snd) $ ts)
