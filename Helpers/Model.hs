module Helpers.Model
    ( joinTables
    , joinJoinTables
    , joinTables3
    , fst3
    , mid3
    , lst3
    ) where
--    ( findOrCreate
--    , joinTables
--    , joinTables3
--    ) where

import Prelude
import Yesod
import Data.Maybe (catMaybes)
import qualified Data.Map as M

-- findOrCreate :: ( YesodPersist m
--                , PersistUnique (YesodPersistBackend m (HandlerT m IO))
--                , PersistEntity v
--                , PersistConfigBackend (YesodPersistBackend m (HandlerT m IO)) ~ PersistEntityBackend v
--                )
--                => v -> HandlerT m IO (Key v)
-- findOrCreate v = return . either entityKey id =<< runDB (insertBy v)

-- |
--
-- My solution to the N+1 problem:
--
-- > runDB $ do
-- >     posts <- selectList [] []
-- >     users <- selectList [] []
-- >
-- >     let records = joinTables postUser posts users
-- >
-- >     forM records $ \(post,user) -> do
-- >         --
-- >         -- ...
-- >         --
--
joinTables :: (PersistEntity b)
            => (a -> Key b)
           -> [Entity a]
           -> [Entity b]
           -> [(Entity a, Entity b)]
joinTables f as bs = catMaybes . for as $ \a -> fmap (\b -> (a,b)) $ lookupRelation f a bs


joinJoinTables :: ((PersistEntity a),
                   (PersistEntity b),
                   (PersistEntity c))
               => (a -> Key b)
               -> (b -> Key c)
               -> [Entity a]
               -> [Entity b]
               -> [Entity c]
               -> [(Entity a, Entity b, Entity c)]
joinJoinTables f g as bs cs = catMaybes . for (joinTables f as bs) $ \(a, b) -> fmap (\c -> (a, b, c)) (lookupRelation g b cs )




joinTables3 :: ((PersistEntity b),
                (PersistEntity c))
            => (a -> Key b)
            -> (a -> Key c)
            -> [Entity a]
            -> [Entity b]
            -> [Entity c]
            -> [(Entity a, Entity b, Entity c)]
joinTables3 f g as bs cs = catMaybes . for as $ \a ->
    case (lookupRelation f a bs, lookupRelation g a cs) of
        (Just b, Just c) -> Just (a,b,c)
        _                -> Nothing

lookupRelation :: (PersistEntity b) => (a -> Key b) -> Entity a -> [Entity b] -> Maybe (Entity b)
lookupRelation f a bs = let k  = f $ entityVal a
                            vs = M.fromList $ map (\(Entity k' v) -> (k',v)) bs
                        in fmap (Entity k) $ M.lookup k vs

for ::  [a] -> (a -> b) -> [b]
for xs f = map f xs

fst3 (x, _, _) = x
mid3 (_, x, _) = x
lst3 (_, _, x) = x
