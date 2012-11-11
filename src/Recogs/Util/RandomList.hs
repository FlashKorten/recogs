-- apfelmus 2009. This code is hereby released into public domain.
module Recogs.Util.RandomList( shuffle ) where

import Control.Monad.Random
    ( StdGen
    , MonadRandom(getRandomR)
    , Rand
    , evalRandIO
    )
import Control.Monad ( liftM )

type R a = Rand StdGen a

-- List returning elements in random order
type RandomList a = R [a]

empty :: RandomList a
empty = return []

singleton :: a -> RandomList a
singleton x = return [x]

-- Fair merge of random lists
merge :: RandomList a -> RandomList a -> RandomList a
merge rxs rys = do
        xs <- rxs
        ys <- rys
        merge' (length xs, xs) (length ys, ys)
    where
      merge' (0 , [])   (_ , ys)   = return ys
      merge' (_ , xs)   (0 , [])   = return xs
      merge' (nx, x:xs) (ny, y:ys) = do
        k <- getRandomR (1, nx + ny)   -- selection weighted by size
        if k <= nx
            then (x:) `liftM` ((nx-1, xs) `merge'` (ny, y:ys))
            else (y:) `liftM` ((nx, x:xs) `merge'` (ny-1, ys))
      merge' (_ , _) _             = error "Impossible happened"

-- Generate a random permutation in O(n log n)
permute :: [a] -> RandomList a
permute = permuteList
    where
    permuteList []  = empty
    permuteList [x] = singleton x
    permuteList xs  = permuteList l `merge` permuteList r
        where (l,r) = splitAt (length xs `div` 2) xs

shuffle :: [a] -> IO [a]
shuffle = evalRandIO . permute
