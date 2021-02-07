module Day04.AdventCoin (findNonce) where

import qualified Crypto.Hash.MD5 as MD5
import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Enc

type Nonce = Int

type Mask = BS.ByteString

findNonce :: Int -> Text.Text -> Nonce
findNonce difficulty text = calculateNonce mask given 1
  where
    given = Enc.encodeUtf8 text
    mask = bsMask difficulty

calculateNonce :: Mask -> BS.ByteString -> Int -> Nonce
calculateNonce mask given n
  | all (== 0) start = n
  | otherwise = calculateNonce mask given (n + 1)
  where
    start = BS.zipWith (.&.) mask hash
    nonce = Enc.encodeUtf8 . Text.pack . show $ n
    hash = MD5.hash $ BS.concat [given, nonce]

bsMask :: Int -> Mask
bsMask difficulty
  | difficulty >= 2 = BS.cons 0xFF (bsMask (difficulty - 2))
  | difficulty == 1 = BS.singleton 0xF0
  | otherwise = BS.empty
