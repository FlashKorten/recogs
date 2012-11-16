{-# LANGUAGE DeriveDataTypeable #-}

module Recogs.Data ( Coord
                   , Game(..)
                   , ConfigParameter(..)
                   , Config(..)
                   , ConfigMap
                   , Dimension
                   ) where

import qualified Data.Map as Map ( Map )
import System.Console.CmdArgs ( Data, Typeable )
import Graphics.UI.SDL ( Surface, Rect )

data Game = Game
            { getRects          :: [Rect]
            , getStep           :: Int
            , getConf           :: Config
            , getImage          :: Surface
            , getBaseOffset     :: Dimension
            , getFileNr         :: Int
            } deriving Show

type Dimension = (Int, Int)
type Coord     = (Int, Int)
type ConfigMap = Map.Map String String

data ConfigParameter = ConfigParameter
                       { cpFile       :: String
                       , cpImagedir   :: Maybe String
                       , cpShuffled   :: Maybe Bool
                       , cpRows       :: Maybe Int
                       , cpCols       :: Maybe Int
                       , cpWidth      :: Maybe Int
                       , cpHeight     :: Maybe Int
                       , cpFullscreen :: Maybe Bool
                       } deriving (Show, Data, Typeable)

data Config = Config
              {configImages :: [String]
              ,configRows   :: Int
              ,configCols   :: Int
              ,configWidth  :: Int
              ,configHeight :: Int
              ,configFS     :: Bool
              } deriving (Show)
