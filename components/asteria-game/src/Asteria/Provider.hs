{- |
Module      : Asteria.Provider
Description : Connect to a running Cardano node via N2C.

Stands up the same channel + 'runNodeClient' plumbing that
@withDevnet@ uses in cardano-node-clients's e2e harness, but
against an *existing* socket instead of spawning its own
@cardano-node@. This is the right shape for both the
docker-compose cluster and the antithesis sandbox.

Usage:

@
withN2C magic socketPath $ \\provider submitter -> do
    pp <- queryProtocolParams provider
    ...
@
-}
module Asteria.Provider (
    withN2C,
    N2CSettings (..),
    settingsFromEnv,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, poll)
import Control.Exception (SomeException, throwIO)
import Data.Word (Word32)
import Text.Read (readMaybe)

import Cardano.Node.Client.N2C.Connection (
    newLSQChannel,
    newLTxSChannel,
    runNodeClient,
 )
import Cardano.Node.Client.N2C.Provider (mkN2CProvider)
import Cardano.Node.Client.N2C.Submitter (mkN2CSubmitter)
import Cardano.Node.Client.Provider (Provider)
import Cardano.Node.Client.Submitter (Submitter)
import Ouroboros.Network.Magic (NetworkMagic (..))
import System.Environment (lookupEnv)

-- | Connection settings resolved from the environment.
data N2CSettings = N2CSettings
    { n2cSocketPath :: FilePath
    , n2cNetworkMagic :: NetworkMagic
    }
    deriving (Eq, Show)

-- | Read 'CARDANO_NODE_SOCKET_PATH' and 'NETWORK_MAGIC'.
settingsFromEnv :: IO N2CSettings
settingsFromEnv = do
    sock <-
        maybe
            (error "CARDANO_NODE_SOCKET_PATH not set")
            id
            <$> lookupEnv "CARDANO_NODE_SOCKET_PATH"
    magicStr <-
        maybe "42" id <$> lookupEnv "NETWORK_MAGIC"
    let magic = case readMaybe magicStr :: Maybe Word32 of
            Just m -> NetworkMagic m
            Nothing ->
                error
                    ( "NETWORK_MAGIC is not a number: "
                        <> magicStr
                    )
    pure
        N2CSettings
            { n2cSocketPath = sock
            , n2cNetworkMagic = magic
            }

{- | Stand up an N2C connection to an existing
@cardano-node@ socket and run an action with a
'Provider' + 'Submitter'.

Mirrors the channel-and-async dance from
@withDevnet@ in @cardano-node-clients@'s e2e
harness, sized for one player loop (queue capacity
16 for both channels).
-}
withN2C ::
    N2CSettings ->
    (Provider IO -> Submitter IO -> IO a) ->
    IO a
withN2C N2CSettings{..} action = do
    lsqCh <- newLSQChannel 16
    ltxsCh <- newLTxSChannel 16
    nodeThread <-
        async $
            runNodeClient
                n2cNetworkMagic
                n2cSocketPath
                lsqCh
                ltxsCh
    -- Same 3-second grace period as @withDevnet@.
    threadDelay 3_000_000
    status <- poll nodeThread
    case status of
        Just (Left e) ->
            throwIO (e :: SomeException)
        Just (Right (Left e)) ->
            throwIO e
        Just (Right (Right ())) ->
            error "asteria-player: N2C closed unexpectedly"
        Nothing -> pure ()
    let provider = mkN2CProvider lsqCh
        submitter = mkN2CSubmitter ltxsCh
    action provider submitter
