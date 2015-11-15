{-# LANGUAGE CPP #-}

module Main where
import Data.Spreadsheet as S
import Control.Monad.Exception.Asynchronous.Lazy as AsExc
import Control.Monad (liftM)
import qualified Data.ByteString.Lazy.UTF8 as U
import Data.Monoid (mempty)
import Data.Char (toUpper)
import Data.List (isSuffixOf)
import Data.Version (showVersion)
import Network.HTTP.Conduit
import Paths_pandoc_placetable (version)
import System.Environment (getArgs)
import Text.Pandoc.JSON
import Text.Pandoc.Definition
import Text.Pandoc.Builder ( Inlines
                           , Blocks
                           , toList
                           , fromList
                           , table
                           , plain
                           , str
                           , divWith )

#if defined(INLINE_MARKDOWN)
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Options
#endif

main :: IO ()
main = do
#if defined(INLINE_MARKDOWN)
  let withInlineMarkdown = "with"
#else
  let withInlineMarkdown = "without"
#endif
  args <- getArgs
  let hasArg a = a `elem` args
  if hasArg "-v" || hasArg "--version" || hasArg "-h" || hasArg "--help"
     then  putStrLn $ unlines [
            "pandoc-placetable " ++ showVersion version
          , "Compiled " ++ withInlineMarkdown ++ " the inlineMarkdown flag."
          , "https://github.com/mb21/pandoc-placetable"
          ]
     else toJSONFilter placeTable

httpConduitManager :: IO Manager
httpConduitManager = newManager tlsManagerSettings

placeTable :: Block -> IO [Block]
placeTable (CodeBlock (ident, cls, kvs) txt) | "table" `elem` cls = do
  csv <- find "file" (return "") getCsv
  let header   = find "header" False (== "yes")
  let inlinemd = find "inlinemarkdown" False (== "yes")
  let aligns   = find "aligns" (repeat AlignDefault) (map toAlign)
  let widths   = find "widths" [] (map read . words)
  let capt     = find "caption" "" id
  let qc       = find "quotechar" '"' head
  let sep      = find "delimiter" ',' $ \d ->
                   if head d == '\\'
                      then case head (tail d) of
                             't' -> '\t'
                             's' -> ' '
                             _   -> '\\'
                      else head d
  let s' = if null txt
              then csv
              else txt ++ "\n" ++ csv
  let s  = if isSuffixOf "\n" s'
              then s'
              else s' ++ "\n"
  let csvTable = csvToTable header inlinemd aligns widths capt qc sep s
  return $ toList $ if null ident
                       then csvTable
                       else divWith (ident,cls,[]) csvTable
  where
    find key def extract = case lookup key kvs of
                             Just x  -> extract x
                             Nothing -> def
    getCsv url = case parseUrl url of
                   Nothing  -> readFile url
                   Just req -> do
                     mgr <- httpConduitManager
                     res <- httpLbs req mgr
                     return $ U.toString $ responseBody res
    toAlign c = case toUpper c of
                  'L' -> AlignLeft
                  'R' -> AlignRight
                  'C' -> AlignCenter
                  _   -> AlignDefault
placeTable a = return [a]


-- | Convert a CSV String to a Pandoc Table
simpleCsvToTable :: String -> Blocks
simpleCsvToTable s = csvToTable False False (repeat AlignDefault) [] mempty '"' ',' s

-- | Convert a bunch of options and a CSV String to a Pandoc Table
csvToTable :: Bool        -- ^ interpret first row as headers
           -> Bool        -- ^ interpret as inline markdown (needs inlineMarkdown compile flag)
           -> [Alignment] -- ^ table column alignments
           -> [Double]    -- ^ table column widths
           -> String      -- ^ table caption
           -> Char        -- ^ csv quotation character like "
           -> Char        -- ^ csv field separator like ,
           -> String      -- ^ csv string to parse
           -> Blocks
csvToTable header inlinemd aligns widths caption qc sep s =
  table (strToInlines caption) cellspecs (map strToBlocks headers)
    $ (map . map) strToBlocks rows
  where
    exc = S.fromString qc sep s
    rows' = case  exception exc of
              Nothing -> result exc
              Just e  -> [["Error parsing CSV: " ++ e]]
    (headers, rows) = if header && length rows' > 0
                         then (head rows', tail rows')
                         else ([], rows')
    nrCols  = if null rows'
                 then 0
                 else length $ head rows'
    widths' = if length widths == nrCols
                 then widths
                 else replicate nrCols 0
    cellspecs = zip aligns widths'

#if defined(INLINE_MARKDOWN)
    strToInlines s =
      if inlinemd
         then
           -- strip newlines and wrap s in a header so only inline syntax is parsed
           let s' = "# " ++ (concat $ lines s)
               extractIns (Header _ _ ins) = ins
               extractIns _ = []
           in  case readMarkdown def s' of
                 Right (Pandoc _ bs) -> fromList $ extractIns $ head bs
                 Left e -> str $ show e
         else
           str s

    strToBlocks s =
      if inlinemd
         then
           case readMarkdown def s of
             Right (Pandoc _ bs) -> fromList bs
             Left e -> plain $ str $ show e
         else
           plain $ str s
#else
    strToInlines s = str s
    strToBlocks  s = plain $ str s
#endif
