{-# LANGUAGE DeriveDataTypeable #-}
module Recogs.Data ( TextureData(..)
                   , Coord
                   , Game(..)
                   , ConfigParameter(..)
                   , Config(..)
                   , ConfigMap
                   ) where

import qualified Data.Map as Map
import Graphics.Rendering.OpenGL
import System.Console.CmdArgs
                                
data TextureData = TextureData
                   { getTextureObject :: TextureObject
                   , getTextureWidth  :: Int
                   , getTextureHeight :: Int
                   } deriving Show

data Game = Game
            { getCoords  :: [Coord]
            , getStep    :: Int
            , getConf    :: Config
            , getTexture :: TextureData
            , getFileNr  :: Int
            } deriving Show

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

