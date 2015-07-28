--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative
import Control.Monad (unless)
import Data.Foldable (for_)
import Data.Maybe
import Data.Monoid (mappend)
import qualified Data.Map as M
import Hakyll
import qualified Text.BibTeX.Entry as E
import qualified Text.BibTeX.Format as F
import Text.BibTeX.Parse (file, skippingLeadingSpace )
import Text.Parsec.String (parseFromFile)
import System.Directory (createDirectoryIfMissing, doesFileExist, getDirectoryContents)
import System.FilePath ((</>), (<.>), takeBaseName, takeExtension)
import System.IO (hPutStrLn, stderr)

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    preprocess $ do
      fs <- filter ((== ".bib") . takeExtension) <$> getDirectoryContents "publications"
      createDirectoryIfMissing True "bibtex"
      for_ fs $ \f -> do
        r <- parseFromFile (skippingLeadingSpace file) ("publications" </> f)
        case r of
          Left err ->
            hPutStrLn stderr ("Error processing file '" ++ f ++ "' (skipping): " ++ show err)
          Right bs ->
            for_ bs $ \b -> do
              let bibFile = "bibtex" </> E.identifier b <.> "bib"
              p <- doesFileExist bibFile
              unless p (writeFile bibFile (F.entry b))

    match ("js/*" .||.
           "lib/**/*" .||.
           "images/**" .||.
           "bibtex/*") $ do
        route idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "*.html" $ do
        route idRoute
        compile $
            getResourceBody >>=
            applyAsTemplate standardContext >>=
            loadAndApplyTemplate "templates/default.html" standardContext >>=
            relativizeUrls

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
