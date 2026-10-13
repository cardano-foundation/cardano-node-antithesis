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
-- The cluster-wide fork depth is the deepest rollback any host would
-- need to follow the longest tip: for each host,
-- @tip[h].chainLength - lca(tip[h], longestTip).chainLength@.  A host
-- whose tip is an ancestor of the longest tip is merely behind, not
-- forked, and needs no rollback.  When the depth crosses Praos's @k@,
-- the divergence is by definition unrecoverable.
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
    , clusterForkDepth

      -- * Internal (exposed for tests)
    , parentOf
    , walkBack
    )
where

import Data.List (delete, find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
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

-- | Record a chain extension on a host: when the new tip is exactly
--   one block above the host's previous tip, the previous tip is its
--   parent.  Idempotent.
--
-- Use this on every @AddedToCurrentChain@ event.  A host that adopts
-- several blocks at once (a node catching up reports only the last
-- block of each batch) skips blocks: the previous tip is then an
-- ancestor but not the parent, and recording it as the parent would
-- overwrite the true edge another host recorded for the same block.
recordExtension :: Host -> Tip -> ForkTreeState -> ForkTreeState
recordExtension host newTip st =
    let st' = case Map.lookup host (tips st) of
            Just prev
                | tipChainLength newTip == tipChainLength prev + 1 ->
                    st
                        { parents =
                            Map.insert
                                (tipHash newTip)
                                (tipHash prev, tipChainLength prev)
                                (parents st)
                        }
            _ -> st
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

-- | Cluster-wide fork depth: the deepest rollback any host would need
--   to follow the longest tip.  For each host that is
--   @tip.chainLength - lca.chainLength@, where @lca@ is the first
--   block of the host's ancestry walk that lies on the longest tip's
--   chain.  A host whose own tip is on that chain is behind, not
--   forked, and contributes @0@.  Both walks are capped at
--   @maxDepth@ ancestors; a host whose walk meets no block of the
--   longest chain within the cap (a gap in the recorded edges, or a
--   lag beyond the cap) contributes nothing.  Ties for the longest
--   tip go to the smallest hash.
--
--   Returns 'Nothing' when no tips are known, or when other hosts
--   exist but none of them can be placed against the longest chain.
clusterForkDepth :: Int -> ForkTreeState -> Maybe Int
clusterForkDepth maxDepth st = case Map.elems (tips st) of
    [] -> Nothing
    allTips ->
        let leader = foldl1 longer allTips
            others = delete leader allTips
            leaderChain =
                Set.fromList
                    . map fst
                    $ walkBack
                        maxDepth
                        (tipHash leader)
                        (tipChainLength leader)
                        st
            rollback t =
                (\(_, len) -> tipChainLength t - len)
                    <$> find
                        ((`Set.member` leaderChain) . fst)
                        (walkBack maxDepth (tipHash t) (tipChainLength t) st)
        in  case (others, mapMaybe rollback others) of
                ([], _) -> Just 0
                (_, []) -> Nothing
                (_, depths) -> Just (maximum depths)
  where
    longer a b
        | tipChainLength b > tipChainLength a = b
        | tipChainLength b == tipChainLength a && tipHash b < tipHash a = b
        | otherwise = a
