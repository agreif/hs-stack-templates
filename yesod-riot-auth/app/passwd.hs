import Prelude
import System.Environment
import Control.Monad
import Data.Text
import qualified Crypto.PasswordStore as Crypto
import qualified Data.ByteString.Char8 as BSC
import Data.Text.Encoding

main :: IO ()
main = do
  args <- getArgs
  forM_ args $ \arg -> do
    crypted <- cryptoHashText $ pack arg
    putStrLn $ unpack crypted

cryptoHashText :: Text -> IO Text
cryptoHashText text = do
  strHash <- Crypto.makePassword (BSC.pack $ unpack text) 17
  return $ decodeUtf8 strHash
