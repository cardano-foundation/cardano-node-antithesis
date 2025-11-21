module Adversary.Application
    ( Limit (..)
    , adversaryApplication
    )
where

import Adversary.ChainSync.Codec (Header, Point, Tip)
import Adversary.ChainSync.Connection (clientChainSync)
import Control.Concurrent.Class.MonadSTM.Strict
    ( MonadSTM (..)
    , StrictTVar
    , newTVarIO
    , readTVar
    , readTVarIO
    , writeTVar
    )
import Control.Exception (Exception, try)
import Data.Maybe (fromMaybe)
import Data.Word (Word32)
import Network.Socket (PortNumber)
import Ouroboros.Consensus.Protocol.Praos.Header ()
import Ouroboros.Consensus.Shelley.Ledger.NetworkProtocolVersion ()
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Network.Block
    ( castPoint
    )
import Ouroboros.Network.Magic (NetworkMagic)
import Ouroboros.Network.Mock.Chain (Chain)
import Ouroboros.Network.Mock.Chain qualified as Chain
import Ouroboros.Network.NodeToNode
    ( ControlMessage (..)
    , ControlMessageSTM
    )
import Ouroboros.Network.Protocol.ChainSync.Client
    ( ChainSyncClient (..)
    , ClientStIdle (..)
    , ClientStIntersect (..)
    , ClientStNext (..)
    )

data Client header point tip m t = Client
    { onRollBackward
        :: point
        -> tip
        -> m (Either t (Client header point tip m t))
    , onRollForward :: header -> m (Either t (Client header point tip m t))
    , points :: [point] -> m (Either t (Client header point tip m t))
    }

newtype Limit = Limit {limit :: Word32}
    deriving newtype (Show, Read, Eq, Ord)

controlledClient
    :: (MonadSTM m)
    => ControlMessageSTM m
    -> Client header point tip m ()
controlledClient controlMessageSTM = client
  where
    client =
        Client
            { onRollBackward = \_ _ -> do
                ctrl <- atomically controlMessageSTM
                case ctrl of
                    Continue -> pure (Right client)
                    Quiesce ->
                        error
                            "Ouroboros.Network.Protocol.ChainSync.Examples.controlledClient: unexpected Quiesce"
                    Terminate -> pure (Left ())
            , onRollForward = \_ -> do
                ctrl <- atomically controlMessageSTM
                case ctrl of
                    Continue -> pure (Right client)
                    Quiesce ->
                        error
                            "Ouroboros.Network.Protocol.ChainSync.Examples.controlledClient: unexpected Quiesce"
                    Terminate -> pure (Left ())
            , points = \_ -> pure (Right client)
            }

terminateAfterCount
    :: StrictTVar IO (Chain Header) -> Limit -> STM IO ControlMessage
terminateAfterCount chainvar limit = do
    chainLength <- getChainLength chainvar
    pure
        $ if chainLength < limit
            then Continue
            else Terminate

getChainLength :: StrictTVar IO (Chain Header) -> STM IO Limit
getChainLength chainvar = Limit . fromIntegral . Chain.length <$> readTVar chainvar

chainSyncClient
    :: StrictTVar IO (Chain Header)
    -> Point
    -> Limit
    -> ChainSyncClient Header Point Tip IO ()
chainSyncClient chainvar startingPoint limit =
    ChainSyncClient
        $ either SendMsgDone initialise <$> getChainPoints
  where
    initialise client' =
        SendMsgFindIntersect [startingPoint]
            $
            -- In this consumer example, we do not care about whether the server
            -- found an intersection or not. If not, we'll just sync from genesis.
            --
            -- Alternative policies here include:
            --  iteratively finding the best intersection
            --  rejecting the server if there is no intersection in the last K blocks
            --
            ClientStIntersect
                { recvMsgIntersectFound = \_ _ -> ChainSyncClient (return (requestNext client'))
                , recvMsgIntersectNotFound = \_ -> ChainSyncClient (return (requestNext client'))
                }

    requestNext client' =
        SendMsgRequestNext
            -- We have the opportunity to do something when receiving
            -- MsgAwaitReply. In this example we don't take up that opportunity.
            (pure ())
            (handleNext client')

    handleNext client' =
        ClientStNext
            { recvMsgRollForward = \header _tip -> ChainSyncClient $ do
                rollForward header
                choice <- onRollForward client' header
                pure $ case choice of
                    Left a -> SendMsgDone a
                    Right client'' -> requestNext client''
            , recvMsgRollBackward = \pIntersect tip -> ChainSyncClient $ do
                rollBackward pIntersect
                choice <- onRollBackward client' pIntersect tip
                pure $ case choice of
                    Left a -> SendMsgDone a
                    Right client'' -> requestNext client''
            }

    getChainPoints :: IO (Either () (Client Header Point Tip IO ()))
    getChainPoints = do
        choice <-
            points (controlledClient $ terminateAfterCount chainvar limit) []
        pure $ case choice of
            Left a -> Left a
            Right client' -> Right client'

    rollForward b = atomically $ do
        chain <- readTVar chainvar
        let !chain' = Chain.addBlock b chain
        writeTVar chainvar chain'

    rollBackward p = atomically $ do
        chain <- readTVar chainvar

        -- We will fail to roll back iff `p` doesn't exist in `chain`
        -- This will happen when we're asked to roll back to `startingPoint`,
        -- which we can check for, or any point before, which we can't
        -- check for. Hence we ignore all failures to rollback and replace the
        -- chain with an empty one if we do.
        let !chain' = fromMaybe Chain.Genesis $ Chain.rollback (castPoint p) chain

        writeTVar chainvar chain'

adversaryApplication
    :: Exception a
    => NetworkMagic
    -> String
    -> PortNumber
    -> Point
    -> Limit
    -> IO (Either a (Chain.Point Header))
adversaryApplication magic peerName peerPort startingPoint limit = do
    chainvar <- newTVarIO (Chain.Genesis :: Chain Header)
    res <-
        -- To gracefully handle the node getting killed it seems we need
        -- the outer 'try', even if connectToNode already returns 'Either
        -- SomeException'.
        try
            $ clientChainSync
                magic
                peerName
                peerPort
                (const $ chainSyncClient chainvar startingPoint limit)
    case res of
        Left e -> return $ Left e
        Right _ -> pure . Chain.headPoint <$> readTVarIO chainvar
