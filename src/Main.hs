{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import           Data.Default        (def)
import           Data.Foldable       (sequenceA_)
import           Data.List           (elemIndex, splitAt)
import           Data.Monoid         ((<>))
import           Data.String         (fromString)
import qualified Options.Applicative as O
import qualified System.Environment  as Sys
import           System.Process      (system)
import qualified Twitch              as T

main :: IO ()
main = mainWithArgs =<< parseArgs =<< Sys.getArgs

parseArgs :: [String] -> IO Options
parseArgs args = O.handleParseResult $ O.execParserPure O.defaultPrefs opts args
  where
    opts = O.info (O.helper <*> cmdOpts)
      (  O.fullDesc
      <> O.progDesc "Watch for file patterns and run commands when they change"
      )

mainWithArgs :: Options -> IO ()
mainWithArgs opts@Options{_patterns} =
  T.defaultMainWithOptions (twitchOpts opts)
    $ sequenceA_ (toDep <$> _patterns)
  where
    toDep (Pattern pat cmd) = T.addModify
      (\file -> system $ "FILE='" ++ file ++ "';" ++ cmd)
      (fromString pat)

twitchOpts :: Options -> T.Options
twitchOpts Options{_dir, _noDebounce} = def{
    T.root=Just _dir,
    T.debounce=if _noDebounce then T.NoDebounce else T.DebounceDefault
  }

data Options = Options{
    _dir        :: String,
    _noDebounce :: Bool,
    _patterns   :: [Pattern]
  } deriving Show

data Pattern = Pattern{
    _pattern :: String,
    _command :: String
  } deriving Show

cmdOpts :: O.Parser Options
cmdOpts = Options
  <$> O.strOption
    (  O.long "dir"
    <> O.short 'd'
    <> O.metavar "DIR"
    <> O.help "Base directory to watch"
    <> O.value "."
    <> O.showDefault
    )
  <*> O.switch
    (  O.long "no-debounce"
    <> O.help "Disable debounce (this is useful if you want a command to run \
              \for each file that changes)"
    <> O.showDefault
    )
  <*> O.many (O.option (readWith parsePattern)
    (  O.long "pattern"
    <> O.short 'p'
    <> O.metavar "PATTERN:COMMAND"
    <> O.help "A glob pattern that matches files paired with a command to run \
              \when any matching file changes. Separate the pattern and the \
              \command with ':'. The command will have access to an environment \
              \variable named $FILE which will contain the path to the file that \
              \changed. Note that if many files change quickly, the debounce \
              \setting may cause only one or some of the files to trigger the \
              \command to run. If you need per-file commands, disable debounce. \
              \(Examples: '**/*.pyo:rm $FILE', '*.cabal:cabal configure', \
              \'**/**:git status')"
    ))

parsePattern :: String -> Either String Pattern
parsePattern str
  | null pat || null cmd = Left "Invalid pattern"
  | otherwise =  Right (Pattern pat cmd)
  where
    idx' = elemIndex ':' str
    (pat, cmd) = case idx' of
      Just idx -> let (pat', cmd') = splitAt idx str
                   in if null cmd'
                        then (pat', "")
                        else (pat', tail cmd')  -- drop leading ':'
      Nothing  -> ("", "")

readWith :: Show err => (String -> Either err b) -> O.ReadM b
readWith f = O.eitherReader (either (Left . show) Right . f)
