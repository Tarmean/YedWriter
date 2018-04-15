{-# Language TemplateHaskell, TypeFamilies, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}
{-# Language GeneralizedNewtypeDeriving #-}
module Types (module Types, module Control.Lens, Text, module Data.String, module Data.Text.Strict.Lens)where
import Control.Lens
import Data.String
import Data.Text (Text)
import Data.Monoid
import Data.Text.Strict.Lens
newtype Ident = Ident { _unNode :: Text } deriving (Show, IsString, Monoid, Eq, Ord)
newtype Multiplicity = Multiplicity { _unMultiplicity :: Text } deriving (Show, IsString, Eq)
makeWrapped ''Ident
makeWrapped ''Multiplicity

class Default a where
    defs :: a
data EdgeKind = Primary | Foreign | Super | Attr | Owned deriving (Show, Eq)

data Connection = Connection
    { _connectionTarget :: Ident
    , _connectionMultiplicity :: Multiplicity
    , _connectionKind :: EdgeKind
    } deriving (Show, Eq)
makeFields ''Connection
data Entity  = Entity
    { _entityIdent :: Ident
    , _entityConnections :: [Connection]
    } deriving (Show, Eq)
makeFields ''Entity

isRelationship :: Entity -> Bool 
isRelationship = anyOf (connections . each . kind) (==Foreign)

data EdgeConfig = EdgeConfig
    { _edgeConfigSource :: Ident
    , _edgeConfigTarget :: Ident
    , _edgeConfigMultiplicity :: Multiplicity
    , _edgeConfigArrow :: Text
    }
makeFields ''EdgeConfig

data NodeConfig = NodeConfig
    { _nodeConfigIdent :: Ident
    , _nodeConfigLabel :: Text
    , _nodeConfigMultiplicity :: Multiplicity
    , _nodeConfigKind :: Text
    , _nodeConfigUnderlined :: Bool
    }
makeFields ''NodeConfig


instance Default Connection where
    defs = Connection
         { _connectionKind = error "edge kind not set"
         , _connectionTarget = error "Connection target not set"
         , _connectionMultiplicity = error "Multiplicity not set"
         }
instance Default EdgeConfig  where
    defs = EdgeConfig
         { _edgeConfigMultiplicity = error "Multiplicity not set"
         , _edgeConfigSource = error "Source not set"
         , _edgeConfigTarget = error "Target not set"
         , _edgeConfigArrow = "none"
         }
instance Default NodeConfig  where
    defs = NodeConfig
         { _nodeConfigKind = error "Node kind not set"
         , _nodeConfigUnderlined = False
         , _nodeConfigMultiplicity = error "Multiplicity not set"
         , _nodeConfigIdent = error "Ident not set"
         , _nodeConfigLabel = error "Label not set"
         }

