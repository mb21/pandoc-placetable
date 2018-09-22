{-# LANGUAGE CPP #-}

module Main where
import Data.Aeson (encode)
import Data.Spreadsheet as S
import Control.Monad (liftM)
import Control.Monad.Exception.Asynchronous.Lazy as AsExc
import qualified Data.ByteString.Lazy as BS (putStr)
import qualified Data.ByteString.Lazy.UTF8 as U
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty)
import Data.Char (toUpper)
import Data.List (isSuffixOf)
import qualified Data.Text as T
import Data.Version (showVersion)
import Network.HTTP.Conduit
import Paths_pandoc_placetable (version)
import System.Environment (getArgs, getProgName)
import System.Console.GetOpt
import System.Exit (exitWith, ExitCode(..))
import System.IO (hPutStrLn, stderr)
import Text.Pandoc.JSON
import Text.Pandoc.Builder ( Inlines
                           , Blocks
                           , doc
                           , toList
                           , fromList
                           , table
                           , plain
                           , str
                           , divWith )

#if defined(INLINE_MARKDOWN)
import Text.Pandoc.Class (runPure)
import Text.Pandoc.Extensions (getDefaultExtensions)
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Options
#endif


data Options = Options { optAligns         :: [Alignment]  -- ^ table column alignments
                       , optCaption        :: String       -- ^ table caption
                       , optDelimiter      :: Char         -- ^ csv field separator like ,
                       , optHeader         :: Bool         -- ^ interpret first row as headers
                       , optInlineMarkdown :: Bool         -- ^ interpret markdown (needs compile flag)
                       , optCsv            :: Maybe (IO String)
                       , optQuoteChar      :: Char         -- ^ csv quotation character like "
                       , optWidths         :: [Double]     -- ^ table column widths
                       }


defaultOpts :: Options
defaultOpts =  Options { optAligns         = repeat AlignDefault
                       , optCaption        = ""
                       , optDelimiter      = ','
                       , optHeader         = False
                       , optInlineMarkdown = False
                       , optCsv            = Nothing
                       , optQuoteChar      = '"'
                       , optWidths         = []
                       }


options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option [] ["aligns"]
        (ReqArg
          (\arg opt -> do
            let toAlign c = case toUpper c of
                              'L' -> AlignLeft
                              'R' -> AlignRight
                              'C' -> AlignCenter
                              _   -> AlignDefault
            return opt { optAligns = map toAlign arg } )
          "[L|R|C]+")
        "table column alignments"
    , Option [] ["widths"]
        (ReqArg
          (\arg opt -> do
            return opt { optWidths = map read $ words arg } )
          "[DECIMAL ]+")
        "table column widths"
    , Option [] ["header"]
        (OptArg
          (\arg opt -> do
            return opt { optHeader = maybe True isTruthy arg } )
          "BOOL")
        "interpret first row as headers"
    , Option [] ["inlinemarkdown"]
        (OptArg
          (\arg opt -> do
            return opt { optInlineMarkdown = maybe True isTruthy arg } )
          "BOOL")
        "interpret text in CSV as markdown (needs inlineMarkdown compile flag)"
    , Option [] ["caption"]
        (ReqArg
          (\arg opt -> do
            return opt { optCaption = arg } )
          "STRING")
        "table caption"
    , Option [] ["quotechar"]
        (ReqArg
          (\arg opt -> do
            return opt { optQuoteChar = head arg } )
          "CHAR")
        "csv quotation character like \""
    , Option [] ["delimiter"]
        (ReqArg
          (\arg opt -> do
            let d = if head arg == '\\'
                       then case head (tail arg) of
                              't' -> '\t'
                              's' -> ' '
                              _   -> '\\'
                       else head arg
            return opt { optDelimiter = d } )
          "CHAR")
        "csv field separator like ,"
    , Option [] ["csv", "file"]
        (ReqArg
          (\arg opt -> do
            let appendNewline s
                  | isSuffixOf "\n" s  = s
                  | otherwise          = s ++ "\n"
            let getCsv = case parseUrlThrow arg of
                           Nothing  -> readFile arg
                           Just req -> do
                             mgr <- httpConduitManager
                             res <- httpLbs req mgr
                             return $ U.toString $ responseBody res
            return opt { optCsv = Just $ liftM appendNewline getCsv } )
          "FILE")
        "csv file or URL"
    , Option "v" ["version"]
        (NoArg
          (\_ -> do
#if defined(INLINE_MARKDOWN)
            let withInlineMarkdown = "with"
#else
            let withInlineMarkdown = "without"
#endif
            hPutStrLn stderr $ unlines [
                "pandoc-placetable " ++ showVersion version
              , "Compiled " ++ withInlineMarkdown ++ " the inlineMarkdown flag."
              , "https://github.com/mb21/pandoc-placetable"
              ]
            exitWith ExitSuccess ))
        "Print version"
    , Option "h" ["help"]
       (NoArg
         (\_ -> do
           prg <- getProgName
           hPutStrLn stderr (usageInfo prg options)
           exitWith ExitSuccess))
       "Show help"
    ]
  where
    isTruthy x = elem x ["yes", "1", "true", "TRUE"]


main :: IO ()
main = do
  rawArgs <- getArgs
  (opts, args) <- rawArgsToOpts rawArgs

  if null args
     then do
       -- either read csv from stdin or if --file option provided form there
       s <- fromMaybe getContents (optCsv opts)
       BS.putStr $ encode $ doc $ csvToTable opts s
     else
       -- if argument provided, assume we are being run as a filter by pandoc
       toJSONFilter placeTable


rawArgsToOpts :: [String] -> IO (Options, [String])
rawArgsToOpts rawArgs = do
  let (actions, args, _) = getOpt Permute options rawArgs

  -- thread defaultOpts through all supplied option actions
  opts <- foldl (>>=) (return defaultOpts) actions

  return (opts, args)


-- flatten key-value tuples to commandline argument-like strings
kvsToArgs :: [(String, String)] -> [String]
kvsToArgs [] = []
kvsToArgs ((k,v):rest) = ("--" ++ k) : v : (kvsToArgs rest)


httpConduitManager :: IO Manager
httpConduitManager = newManager tlsManagerSettings


placeTable :: Block -> IO [Block]
placeTable (CodeBlock (ident, cls, kvs) txt) | "table" `elem` cls = do
  (opts, _) <- rawArgsToOpts $ kvsToArgs kvs
  csv  <- fromMaybe (return "") (optCsv opts)
  let s = if null txt
             then csv
             else txt ++ "\n" ++ csv
  let csvTable = csvToTable opts s
  return $ toList $ if null ident && null kvs'
                       then csvTable
                       else divWith (ident,cls,kvs') csvTable
  where
    kvs' = filter (\(k,_) -> not $ elem k $ concatMap getNames options) kvs
    getNames (Option _ ns _ _) = ns
placeTable a = return [a]


-- | Convert options and a CSV String to a Pandoc Table
csvToTable :: Options -> String -> Blocks
csvToTable opts csv =
  table (strToInlines $ optCaption opts) cellspecs (map strToBlocks headers)
    $ (map . map) strToBlocks rows
  where
    exc = S.fromString (optQuoteChar opts) (optDelimiter opts) csv
    rows' = case  exception exc of
              Nothing -> result exc
              Just e  -> [["Error parsing CSV: " ++ e]]
    (headers, rows) = if optHeader opts && length rows' > 0
                         then (head rows', tail rows')
                         else (replicate nrCols [], rows')
    nrCols  = if null rows'
                 then 0
                 else length $ head rows'
    calculatedWidth = if optInlineMarkdown opts
                         then 1.0 / fromIntegral nrCols
                         else 0 --simulate simple_table
    widths  = optWidths opts
    widths' = if length widths == nrCols
                 then widths
                 else replicate nrCols calculatedWidth
    cellspecs = zip (optAligns opts) widths'

#if defined(INLINE_MARKDOWN)
    readerOpts = def {readerExtensions = getDefaultExtensions "markdown"}
    strToInlines s =
      if optInlineMarkdown opts
         then
           -- strip newlines and wrap s in a header so only inline syntax is parsed
           let s' = "# " ++ (concat $ lines s)
               extractIns (Header _ _ ins) = ins
               extractIns _ = []
           in  case runPure (readMarkdown readerOpts $ T.pack s') of
                 Right (Pandoc _ bs) -> fromList $ extractIns $ head bs
                 Left e -> str $ show e
         else
           if null s
              then mempty
              else str s

    strToBlocks s =
      if optInlineMarkdown opts
         then
           case runPure (readMarkdown readerOpts $ T.pack s) of
             Right (Pandoc _ bs) -> fromList bs
             Left e -> plain $ str $ show e
         else
           if null s
              then mempty
              else plain $ str s
#else
    strToInlines [] = mempty
    strToInlines s  = str s
    strToBlocks  [] = mempty
    strToBlocks  s  = plain $ str s
#endif
