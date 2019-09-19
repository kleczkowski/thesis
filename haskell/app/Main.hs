module Main
  ( main
  )
where

import           Codec.AC
import           Options.Applicative
import           System.IO

data Opts 
  = Encode Input Output
  | Decode Input Output

data Input
  = FileInput FilePath
  | StdIn

data Output
  = FileOutput FilePath
  | StdOut

optsParser :: Parser Opts
optsParser = subparser
  (  command "encode" (info encodeParser (fullDesc <> progDesc "Run arithmetic enoder"))
  <> command "decode" (info decodeParser (fullDesc <> progDesc "Run arithmetic decoder"))
  )

encodeParser :: Parser Opts
encodeParser = Encode <$> inputParser <*> outputParser

decodeParser :: Parser Opts
decodeParser = Decode <$> inputParser <*> outputParser

inputParser :: Parser Input
inputParser = fileInputParser <|> stdinParser
  where
    fileInputParser = FileInput
      <$> argument str
          (  metavar "INPUT_FILE"
          <> help "Input file path" )
    stdinParser = flag' StdIn
      (  long "stdin"
      <> help "read content from standard input" )

outputParser :: Parser Output
outputParser = fileOutputParser <|> stdoutParser
  where
    fileOutputParser = FileOutput
      <$> argument str
        (  metavar "OUTPUT_FILE"
        <> help "Output file path" )
    stdoutParser = flag' StdOut
      (  long "stdout"
      <> help "write content to standard output" )


getInputHandle :: Input -> IO Handle
getInputHandle (FileInput path) = openFile path ReadMode
getInputHandle StdIn = return stdin

getOutputHandle :: Output -> IO Handle
getOutputHandle (FileOutput path) = openFile path WriteMode
getOutputHandle StdOut = return stdout

run :: Opts -> IO ()
run (Encode input output) = do
  hIn <- getInputHandle input
  hOut <- getOutputHandle output
  hCompress hIn hOut
  hClose hIn
  hClose hOut
run (Decode input output) = do
  hIn <- getInputHandle input
  hOut <- getOutputHandle output
  hDecompress hIn hOut
  hClose hIn
  hClose hOut

main :: IO ()
main = run =<< execParser opts 
  where
    opts = info (optsParser <**> helper)
      (  fullDesc
      <> progDesc "Compress data with the arithmetic codec"
      <> header "ac-haskell --- arithmetic codec written in Haskell" )
