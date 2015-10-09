import qualified Data.Map.Strict as M
import qualified Data.List as L
import Control.Applicative
import Data.Maybe

data Trie a = Trie { children :: M.Map a (Trie a), end :: Bool }

tEmpty = Trie M.empty False

insert :: Ord a => [a] -> Trie a -> Trie a
insert []     (Trie m e) = Trie m True
insert (x:xs) (Trie m e) = Trie (M.alter (Just . insert xs . fromMaybe tEmpty) x m) e
        
contains :: Ord a => [a] -> Trie a -> Bool
contains []     = end
contains (x:xs) = fromMaybe False   . 
                  (contains xs <$>) . 
                  M.lookup x        . 
                  children
        
fromList :: Ord a => [[a]] -> Trie a
fromList = foldr insert tEmpty

prepResult :: (b -> [[a]]) -> (a,b) -> [[a]]
prepResult f (a,b) = fmap (a:) (f b)

allWords :: Trie a -> [[a]]
allWords (Trie m e) | e = []:rest
                    | otherwise = rest
  where rest = (M.toList m) >>= prepResult allWords
  
complete :: Ord a => [a] -> Trie a -> [[a]]
complete []     = allWords
complete (x:xs) = fromMaybe []                     . 
                  (fmap $ fmap (x:) . complete xs) . 
                  M.lookup x                       . 
                  children
                
subs :: Ord a => [a] -> Trie a -> [[a]]
subs x t@(Trie m _) = (complete x t) ++ (M.toList m >>= prepResult (subs x))
