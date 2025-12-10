module Adversary.SubmitTransactionsSpec where

import Adversary.SubmitTransactions (fromHex, getTxId, mkGenTx, mkTxId, pollTransactionsFromFiles)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (cancel, withAsync)
import Control.Concurrent.Class.MonadSTM.Strict (atomically, newTBQueueIO)
import Control.Concurrent.Class.MonadSTM.Strict.TBQueue (tryReadTBQueue)
import Control.Exception (bracket)
import Control.Monad (replicateM)
import Data.ByteString.Lazy qualified as LBS
import Data.Either (fromRight)
import Data.Maybe (catMaybes)
import GHC.Stack (HasCallStack)
import System.Directory (createDirectory, removePathForcibly, canonicalizePath)
import System.FilePath ((</>))
import System.IO (hClose)
import System.Posix (mkstemp)
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = do
  let txBytes :: LBS.ByteString = fromHex "84a500818258201efc7ffb9772f4425a8edea129b6f08732d3dc41fcb957bfd40d0db68bb33d1600018182581d609749ac0e073844185403a2927239b4e663085e7090d6a4691ac1decf1a00209514021a0005908c031a7fffffff048282008200581c89ca2be8a9b0680dd3293268fe336bf46e2992e52d551b4bdb624a1283028200581c89ca2be8a9b0680dd3293268fe336bf46e2992e52d551b4bdb624a12581c040e1032e2dda149c99fc9d45f06a0457ded6958cd5b2fc2a952ea6ea100828258208c7cc0fcc56e445e28554694060ab5a73c44ceeb76b09530f7126f49f99a63c65840a859678f5d78d8dba62a3f40bc333d4537ae9353de4db0678de8c58158fc4df47185f01cac722133d1f50c465e6ec6155b6763a94acec660c4243f1082bc5b0f8258201751b10751f407febe1f9482514195546e13903e6d63ed2260d03ceb28dcff92584075c78c766d635e4d3953f370af5d2303bb16a25b2f3e59f9a1acd09a293719ff76d2aedc96a7ffa345f8c12faf595895998293087c07e43675b0c1c99b102401f5f6"

  it "can compute transaction id from raw bytes" $ do
    let txId = mkTxId txBytes
    let expectedTxId = getTxId <$> mkGenTx txBytes
    txId `shouldBe` expectedTxId

  it "can collect transactions from a directory as they are created" $ do
    let txId = fromRight undefined $ mkTxId txBytes
    queue <- newTBQueueIO 10
    withTempDir $ \dir -> do
      withAsync (pollTransactionsFromFiles [dir] queue) $ \async -> do
        threadDelay 100000
        let txFile1 = dir </> "tx1"
        let txFile2 = dir </> "tx2"
        LBS.writeFile txFile1 txBytes
        LBS.writeFile txFile2 txBytes
        threadDelay 1000000 -- wait for 1 second to allow polling
        cancel async

    txs <- catMaybes <$> atomically (replicateM 2 (tryReadTBQueue queue))

    length txs `shouldBe` 2
    fst <$> txs `shouldBe` [txId, txId]

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir =
  bracket (mkTempFile >>= (\fp -> removePathForcibly fp >> createDirectory fp >> canonicalizePath fp)) (const $ pure ()) -- removePathForcibly

mkTempFile :: IO FilePath
mkTempFile = mkstemp "test-adversary" >>= \(fp, h) -> hClose h >> pure fp
