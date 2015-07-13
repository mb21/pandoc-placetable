{-# LANGUAGE CPP #-}

module Main where
import Data.Spreadsheet as S
import Control.Monad.Exception.Asynchronous.Lazy as AsExc
import Control.Monad (liftM)
import Data.Monoid (mempty)
import Data.Char (toUpper)
import Data.List (isSuffixOf)
import Text.Pandoc.JSON
import Text.Pandoc.Definition
import Text.Pandoc.Builder (Inlines, Blocks, toList, fromList, table, para, str)

#if defined(INLINE_MARKDOWN)
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Options
#endif


main :: IO ()
main = toJSONFilter placeTable

placeTable :: Block -> IO [Block]
placeTable (CodeBlock (_, cls, kvs) txt) | "table" `elem` cls = do
  csv <- find "file" (return "") readFile
  let header   = find "header" False (== "yes")
  let inlinemd = find "inlinemarkdown" False (== "yes")
  let aligns   = find "aligns" (repeat AlignDefault) (map toAlign)
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
  return $ toList $ csvToTable header inlinemd aligns capt qc sep s
  where
    find key def extract = case lookup key kvs of
                             Just x  -> extract x
                             Nothing -> def
    toAlign c = case toUpper c of
                  'L' -> AlignLeft
                  'R' -> AlignRight
                  'C' -> AlignCenter
                  _   -> AlignDefault
placeTable a = return [a]


-- | Convert a CSV String to a Pandoc Table
simpleCsvToTable :: String -> Blocks
simpleCsvToTable s = csvToTable False False (repeat AlignDefault) mempty '"' ',' s

-- | Convert a bunch of options and a CSV String to a Pandoc Table
csvToTable :: Bool        -- ^ interpret first row as headers
           -> Bool        -- ^ interpret as inline markdown (needs inlineMarkdown compile flag)
           -> [Alignment] -- ^ table column alignments
           -> String      -- ^ table caption
           -> Char        -- ^ csv quotation character like "
           -> Char        -- ^ csv field separator like ,
           -> String      -- ^ csv string to parse
           -> Blocks
csvToTable header inlinemd aligns caption qc sep s =
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
    cols = if null rows' then 0 else length $ head rows'
    cellspecs = zip aligns $ replicate cols 0

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
             Left e -> para $ str $ show e
         else
           para $ str s
#else
    strToInlines s = str s
    strToBlocks  s = para $ str s
#endif
