{- |
Module      : Asteria.Deploy
Description : Read\/write the per-deploy seed 'TxIn' on disk.

Bootstrap picks a seed UTxO at runtime, applies it to the four
validators, and writes the seed to a JSON file before submitting
the deploy tx. Player and invariant binaries read that file on
startup so they all derive the same per-deploy script hashes /
addresses as the bootstrap.

The file lives on a small dedicated volume mounted into every
asteria-game container; @ASTERIA_DEPLOY_DIR@ overrides the path
(default @\/asteria-deploy@).
-}
module Asteria.Deploy (
    deployDir,
    seedFilePath,
    readSeed,
    writeSeed,
) where

import Control.Exception (IOException, try)
import Data.Aeson (
    FromJSON (..),
    ToJSON (..),
    eitherDecodeFileStrict',
    encodeFile,
    object,
    withObject,
    (.:),
    (.=),
 )
import Data.Aeson.Types (Parser)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BS8
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (lookupEnv)
import System.FilePath ((</>))

import Cardano.Crypto.Hash.Class (hashFromBytes, hashToBytes)
import Cardano.Ledger.BaseTypes (TxIx (..))
import Cardano.Ledger.Hashes (extractHash, unsafeMakeSafeHash)
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))

deployDir :: IO FilePath
deployDir = fromMaybe "/asteria-deploy" <$> lookupEnv "ASTERIA_DEPLOY_DIR"

seedFilePath :: IO FilePath
seedFilePath = (</> "seed.json") <$> deployDir

newtype SeedFile = SeedFile {seedFileTxIn :: TxIn}

instance ToJSON SeedFile where
    toJSON (SeedFile (TxIn (TxId h) (TxIx ix))) =
        let bs = hashToBytes (extractHash h)
            hex = T.pack (BS8.unpack (Base16.encode bs))
         in object
                [ "seed_tx_id" .= hex
                , "seed_tx_idx" .= ix
                ]

instance FromJSON SeedFile where
    parseJSON = withObject "SeedFile" $ \o -> do
        hex <- o .: "seed_tx_id" :: Parser Text
        ixWord <- o .: "seed_tx_idx" :: Parser Word64
        case Base16.decode (BS8.pack (T.unpack hex)) of
            Left e -> fail ("seed_tx_id hex decode: " <> e)
            Right bs -> case hashFromBytes bs of
                Nothing -> fail "seed_tx_id wrong length"
                Just h ->
                    pure $
                        SeedFile $
                            TxIn
                                (TxId (unsafeMakeSafeHash h))
                                (TxIx (fromIntegral ixWord))

readSeed :: IO (Maybe TxIn)
readSeed = do
    path <- seedFilePath
    exists <- doesFileExist path
    if not exists
        then pure Nothing
        else do
            r <- try (eitherDecodeFileStrict' path)
            case r :: Either IOException (Either String SeedFile) of
                Left e -> error ("Asteria.Deploy.readSeed: " <> show e)
                Right (Left e) -> error ("Asteria.Deploy.readSeed: " <> e)
                Right (Right s) -> pure (Just (seedFileTxIn s))

{- | Write the seed to disk. The caller (bootstrap) MUST call
this before submitting the deploy tx, so a crash between the
two leaves either no file (next attempt picks fresh seed) or a
consistent file (next attempt re-derives the same scripts).
-}
writeSeed :: TxIn -> IO ()
writeSeed seed = do
    dir <- deployDir
    createDirectoryIfMissing True dir
    path <- seedFilePath
    encodeFile path (SeedFile seed)
