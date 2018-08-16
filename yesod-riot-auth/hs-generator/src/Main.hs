{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as Text
import qualified Text.Ginger as G
import qualified System.IO as SIO (IOMode(ReadMode), hSetEncoding, withFile, utf8_bom)
import qualified System.IO.Strict as SIOS
import qualified System.IO.Error as SIOE
import qualified Options.Applicative as O
import qualified GHC.IO.Encoding as E
import Data.Semigroup ((<>))

import Custom

main :: IO ()
main = do
  E.setLocaleEncoding E.utf8
  templFile <- O.execParser argInfo
  template <- loadTemplate templFile
  putStrLn . Text.unpack $ G.easyRender context template

-- command line parser

argParser :: O.Parser String
argParser = O.argument O.str (O.metavar "TEMPLATE" <> O.help "Ginger template file")

argInfo :: O.ParserInfo String
argInfo = O.info (argParser O.<**> O.helper)
  ( O.fullDesc
  <> O.progDesc "Generate code from TEMPLATE"
  <> O.header "hs-generator - yesod partioal code generator" )

-- ginger

loadTemplate :: String -> IO (G.Template G.SourcePos)
loadTemplate templFile = do
  eitherTemplate <- G.parseGingerFile' opts templFile
  return $ case eitherTemplate of
             Left err -> error . show $ err
             Right template' -> template'

opts :: G.ParserOptions IO
opts = (G.mkParserOptions fileResolver) { G.poSourceName = Nothing
                                        , G.poKeepTrailingNewline = True }

fileResolver :: G.IncludeResolver IO
fileResolver filename = do
  content <- loadFile filename
  return $ Just content

loadFile :: FilePath -> IO String
loadFile fn =
  SIOE.tryIOError (loadFile' $ "ginger/" ++ fn) >>= \e ->
    case e of
      Right contents -> return contents
      Left err -> return $ show err
  where
    loadFile' :: FilePath -> IO String
    loadFile' fn' = do
      SIO.withFile fn' SIO.ReadMode $ \h -> do
        SIO.hSetEncoding h SIO.utf8_bom
        contents <- SIOS.hGetContents h
        return contents
