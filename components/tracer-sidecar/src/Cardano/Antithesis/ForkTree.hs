-- |
-- Module      : Cardano.Antithesis.ForkTree
-- Description : Pure cluster-wide fork tree built from per-node tip events
--
-- For each node we track its current tip; for each block ever adopted
-- by any node we record an edge (block hash → parent hash, chain
-- length).  Edges are sourced from @AddedToCurrentChain@ events: the
-- /producer/ of a block always extends from its own tip, so its event
-- captures the true parent.  Other nodes adopting the same block via
-- @SwitchedToAFork@ do not need to record the parent (the producer
-- already did, eventually).
--
-- The cluster-wide fork depth is then
-- @max(tip[h].chainLength) - commonAncestor.chainLength@ where
-- @commonAncestor@ is the deepest block present in /every/ host's
-- ancestry walk.  When that value crosses Praos's @k@, the divergence
-- is by definition unrecoverable.
module Cardano.Antithesis.ForkTree
    ( -- * Types
      BlockHash
    , ChainLength
    , Host
    , Tip (..)
    , ForkTreeState

      -- * Construction
    , initialForkTreeState

      -- * Updates
    , recordExtension
    , setTip

      -- * Queries
    , currentTips
    , commonAncestor
    , clusterForkDepth

      -- * Internal (exposed for tests)
    , parentOf
    , walkBack
    )
where

import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)

-- | Hex-encoded block hash, the @hash@ component of the
--   @"hash@slot"@ trace strings.
type BlockHash = Text

-- | Chain length (number of blocks since genesis) as reported by
--   the node's chain selection view.
type ChainLength = Int

-- | A node's hostname (from the @host@ field of every trace event).
type Host = Text

-- | A node's current chain tip.
data Tip = Tip
    { tipHash :: !BlockHash
    , tipChainLength :: !ChainLength
    , tipSlotNo :: !Int
    }
    deriving (Show, Eq)

-- | Cluster-wide fork tree state.
--
-- * 'tips' holds the most recent tip observed for each host.
-- * 'parents' maps every adopted block's hash to its parent hash
--   plus the parent's chain length (so walking back is O(depth)
--   without further lookups).
data ForkTreeState = ForkTreeState
    { tips :: !(Map Host Tip)
    , parents :: !(Map BlockHash (BlockHash, ChainLength))
    }
    deriving (Show, Eq)

-- | Initial empty state.
initialForkTreeState :: ForkTreeState
initialForkTreeState =
    ForkTreeState{tips = Map.empty, parents = Map.empty}

-- | Record a sequential chain extension on a host: the previous
--   tip becomes the parent of the new tip.  Idempotent.
--
-- Use this on every @AddedToCurrentChain@ event.  The new tip's
-- parent IS the previous tip the host had, regardless of how the
-- previous tip arrived (forge, normal extension, or fork switch).
recordExtension :: Host -> Tip -> ForkTreeState -> ForkTreeState
recordExtension host newTip st =
    let st' = case Map.lookup host (tips st) of
            Nothing -> st
            Just prev ->
                st
                    { parents =
                        Map.insert
                            (tipHash newTip)
                            (tipHash prev, tipChainLength prev)
                            (parents st)
                    }
    in  st'{tips = Map.insert host newTip (tips st')}

-- | Update a host's current tip without recording a parent edge.
--   Use this on @SwitchedToAFork@ events: the new tip's parent is
--   /not/ the previous tip on this host, and the producer's
--   @AddedToCurrentChain@ has either already recorded the true
--   parent or will arrive shortly.
setTip :: Host -> Tip -> ForkTreeState -> ForkTreeState
setTip host newTip st =
    st{tips = Map.insert host newTip (tips st)}

-- | All known tips, indexed by host.
currentTips :: ForkTreeState -> Map Host Tip
currentTips = tips

-- | Look up a block's parent (hash and chain length).
parentOf
    :: BlockHash -> ForkTreeState -> Maybe (BlockHash, ChainLength)
parentOf h = Map.lookup h . parents

-- | Walk a chain back from a starting hash, capped at @maxDepth@
--   ancestors visited.  Returns the visited @(hash, chainLength)@
--   pairs starting with the input hash.  Stops early if a hash
--   has no recorded parent.
walkBack
    :: Int
    -> BlockHash
    -> ChainLength
    -> ForkTreeState
    -> [(BlockHash, ChainLength)]
walkBack maxDepth startHash startLen st =
    take maxDepth (go startHash startLen)
  where
    go h len =
        (h, len) : case parentOf h st of
            Nothing -> []
            Just (ph, plen) -> go ph plen

-- | Deepest block present in /every/ host's ancestry walk, capped
--   at @maxDepth@ ancestors per host.  Returns 'Nothing' if there
--   are no tips, or if the tips share no recorded common ancestor
--   within the cap.
commonAncestor
    :: Int -> ForkTreeState -> Maybe (BlockHash, ChainLength)
commonAncestor maxDepth st = case Map.elems (tips st) of
    [] -> Nothing
    (firstTip : restTips) ->
        let firstWalk =
                walkBack
                    maxDepth
                    (tipHash firstTip)
                    (tipChainLength firstTip)
                    st
            restWalks =
                [ walkBack maxDepth (tipHash t) (tipChainLength t) st
                | t <- restTips
                ]
            firstSet = Set.fromList (map fst firstWalk)
            restSets = map (Set.fromList . map fst) restWalks
            common = foldl' Set.intersection firstSet restSets
            -- Read each common hash's chain length from firstWalk
            -- (chain length is a property of the block, identical
            -- across all walks that contain it).
            commonWithLen =
                mapMaybe
                    (\(h, l) -> if Set.member h common then Just (h, l) else Nothing)
                    firstWalk
        in  case commonWithLen of
                [] -> Nothing
                (x : xs) -> Just (foldl' deeper x xs)
  where
    deeper a@(_, la) b@(_, lb) = if lb > la then b else a

-- | Cluster-wide fork depth: @max(tip.chainLength) -
--   commonAncestor.chainLength@.  Returns 'Nothing' when no common
--   ancestor is known (e.g.  not enough events processed yet).
clusterForkDepth :: Int -> ForkTreeState -> Maybe Int
clusterForkDepth maxDepth st = do
    (_, ancestorLen) <- commonAncestor maxDepth st
    let lens = map tipChainLength (Map.elems (tips st))
    case lens of
        [] -> Nothing
        _ -> Just (foldl' max 0 lens - ancestorLen)
