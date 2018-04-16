{-# Options_GHC -fdefer-typed-holes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module GraphMLWriter (writeGraph) where
import Types
import Text.XML.HXT.Core as X
import Text.XML.HXT.DOM.QualifiedName (mkQName)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))

writeGraph :: (Maybe String) -> [Entity] -> IO ()
writeGraph outPath input = do
    let
        graph = mapArr mkEntry input
    _ <- runX (root [] [graphXml "test graph" 4 3 [graph] >>> uniqueNamespacesFromDeclAndQNames]
             >>> propagateNamespaces
             >>> writeDocument [withCheckNamespaces yes, withIndent yes] (fromMaybe "-" outPath)
             )
    return ()

graphXml :: ArrowXml a => String -> Int -> Int -> [a n XmlTree] -> a n XmlTree
graphXml graphName nodeCount edgeCount fullGraph = 
  mkqelem 
       (mkQName "" "graphml" "")
       [ "xmlns" =: "http://graphml.graphdrawing.org/xmlns"
       , "xmlns:java" =: "http://www.yworks.com/xml/yfiles-common/1.0/java"
       , "xmlns:sys" =: "http://www.yworks.com/xml/yfiles-common/markup/primitives/2.0"
       , "xmlns:x" =: "http://www.yworks.com/xml/yfiles-common/markup/2.0"
       , "xmlns:xsi" =: "http://www.w3.org/2001/XMLSchema-instance"
       , "xmlns:y" =: "http://www.yworks.com/xml/graphml"
       , "xmlns:yed" =: "http://www.yworks.com/xml/yed/3"
       , "xsi:schemaLocation" =: "http://graphml.graphdrawing.org/xmlns http://www.yworks.com/xml/schema/graphml/1.1/ygraphml.xsd"
       ]
       [ mkKey
           [ "for" =: "node"
           , "id" =: "d0"
           , "yfiles.type" =: "nodegraphics"
           ]
        , mkKey
            [ "for" =: "node"
            , "id" =: "d1"
            , "name" =: "description"
            , "attr.type" =: "string"
            ]
        , mkKey
            [ "for" =: "edge"
            , "id" =: "d2"
            , "yfiles.type" =: "nodegraphics"
            ]
        , mkKey
            [ "for" =: "edge"
            , "id" =: "d3"
            , "name" =: "description"
            , "attr.type" =: "string"
            ]
        , mkKey
            [ "for" =: "graphml"
            , "id" =: "d4"
            , "yfiles.type" =: "resources"
            ]
        , mkKey
            [ "for" =: "node"
            , "id" =: "d6"
            , "yfiles.type" =: "nodegraphics"
            ]
        , mkKey
            [ "for" =: "edge"
            , "id" =: "d9"
            , "attr.type" =: "string"
            , "attr.name" =: "description"
            ]
        , mkKey
            [ "for" =: "edge"
            , "id" =: "d10"
            , "yfiles.type" =: "edgegraphics"
            ]
        , mkelem "graph"
            [ "edgedefault" =: "directed"
            , "parse.order" =: "free"
            , "parse.edges" =: (show $ nodeCount)
            , "parse.nodes" =: (show $ edgeCount)
            , "id" =: graphName]
            fullGraph
        , mkData "d4" [ mkqelem (mkQName "y" "Resources" "") [] [] ]
        ]

mkEntry :: (ArrowXml a) => Entity -> a b XmlTree
mkEntry e
    = mkNode conf
  <+> mkConnections e
  where
    conf = defs
         & ident .~ e^.ident
         & label .~ e^.ident._Wrapped
         & kind .~ kindIdentifier
    kindIdentifier = if isRelationship e
                     then  "com.yworks.entityRelationship.relationship"
                     else "com.yworks.entityRelationship.small_entity"

mkConnections :: (ArrowXml a) => Entity -> a b XmlTree
mkConnections e = mapArr (mkConnection $ e^.ident) (e^.connections)

mkConnection :: (ArrowXml a) => Ident -> Connection -> a b XmlTree
mkConnection name con = case con^.kind of
    Foreign o -> mkLabeledEdge conf
    Super _ -> mkLabeledEdge $ conf & arrow .~ "standard"
    _ -> mkAttribute name con 
  where
    conf = defs 
        & source .~ name
        & multiplicity .~ con^.multiplicity
        & target .~ con^.target

mkAttribute :: ArrowXml a => Ident -> Connection -> a b XmlTree
mkAttribute owner con
    = mkNode (defs
             & ident .~ attrIdent
             & label .~ con^.target._Wrapped
             & kind .~ "com.yworks.entityRelationship.attribute"
             & underlined .~ con^.primary )
  <+> attrEdge
    where
      attrIdent = owner <> "_" <> con^.target
      attrEdge
        | con ^. multiplicity == Optional
            = mkLabeledEdge
            $ defs
            & source .~ attrIdent
            & target .~ owner
            & multiplicity .~ One
            & arrow .~  arrowOptional
        | con ^. multiplicity == One
            = mkLabeledEdge
            $ defs
            & source .~ attrIdent
            & target .~ owner
            & multiplicity .~ One
        | otherwise = error "attributes can't have multiplicity"
       
arrowOptional = "crows_foot_optional"
-- Fricking glorified ListT IO grumble grumble
mapArr :: ArrowPlus cat => (t -> cat a b) -> [t] -> cat a b
mapArr f ls = go ls
  where
    go (x:xs) = f x <+> go xs
    go [] = zeroArrow

mkNode :: ArrowXml a => NodeConfig -> a n XmlTree
mkNode config =
    mkelem "node"
        ["id" =: config^.ident._String]
        [ ynode config
        , mkData "d1" []
        ]

ynode :: ArrowXml a => NodeConfig -> a n XmlTree
ynode config =
    mkData "d0" 
        [ mkGenericNode (config^.kind.unpacked)
            [ mkFill ["color" =: "#E8EEF7", "color2" =: "#B7C9E3", "transparent" =: "false"]
            , mkBorderStyle ["color" =: "#000000", "type" =: "line", "width" =: "1.0"]
            , mkGeometry
                [ "height" =: "40.0"
                , "width"  =: "80.0"
                , "x"      =: "0.0"
                , "y"      =: "0.0"
                ]
            , mkLabel (config^.label.unpacked)
                [ "alignment"          =: "center"
                , "underlinedText" =: if config^.underlined then "true" else "false"
                , "autoSizePolicy"     =: "content"
                , "hasBackgroundColor" =: "false"
                , "modelName"          =: "internal"
                , "modelPosition"      =: "c"
                ]
            , setShadow True
            ]
        ]

mkGenericNode :: ArrowXml a => String -> [a n XmlTree] -> a n XmlTree
mkGenericNode configuration elems = 
        mkqelem genericNode ["configuration" =: configuration] elems
  where genericNode = mkQName "y" "GenericNode" ""

mkData :: ArrowXml a => String -> [a n XmlTree] -> a n XmlTree
mkData key elems = mkelem "data" ["key" =: key] elems

mkFill :: ArrowXml a => [a n XmlTree] -> a n XmlTree
mkFill attrs = mkqelem fill attrs []
  where fill = mkQName "y" "Fill" ""

mkLabel :: ArrowXml a => String -> [a n XmlTree] -> a n XmlTree
mkLabel lbl attrs = mkqelem nodeLabel attrs [txt lbl]
  where nodeLabel       = mkQName "y" "NodeLabel" ""

mkGeometry :: ArrowXml a => [a n XmlTree] -> a n XmlTree
mkGeometry attrs = mkqelem geometry attrs []
  where geometry        = mkQName "y" "Geometry" ""

mkBorderStyle :: ArrowXml a => [a n XmlTree] -> a n XmlTree
mkBorderStyle attrs = mkqelem border attrs []
  where border          = mkQName "y" "BorderStyle" ""

mkProperty :: ArrowXml a => String -> String -> String -> a n XmlTree
mkProperty typ prop value = mkqelem property ["class" =: typ, "name" =: prop, "value" =: value] []
  where property = mkQName "y" "Property" ""

setShadow :: ArrowXml a => Bool -> a n XmlTree
setShadow value = mkProperty "java.lang.Boolean" "y.view.ShadowNodePainter.SHADOW_PAINTING" (if value then "true" else "false")

mkLabeledEdge :: ArrowXml a => EdgeConfig -> a n XmlTree
mkLabeledEdge conf = mkelem "edge"
        [ "source" =: conf^.source._String
        , "target" =: conf^.target._String
        , "id"     =: edgeIdent
        ]
        [mkData "d9" [], mkData "d10" [polyEdge]] 
  where
    edgeIdent = conf^.source._String <> "-" <> conf^.target._String <> conf^.multiplicity.to show
    polyEdge = qelem "PolyLineEdge" []
        [ qelem "Path" ["sx" =: "0.0", "sy" =: "0.0", "tx" =: "0.0", "ty" =:"0.0"] []
        , qelem "LineStyle" ["color"=:"#000000", "type"=:"line", "width"=:"1.0"] []
        , qelem"Arrows"["source"=:"none", "target"=:conf^.arrow.unpacked] []
        , qelem"EdgeLabel"
            [ "alignment"=:"center"
            , "configuration"=:"AutoFlippingLabel"
            , "distance"=:"2.0"
            , "fontFamily"=:"Dialog"
            , "fontSize"=:"12"
            , "fontStyle"=:"plain"
            , "hasBackgroundColor"=:"false"
            , "hasLineColor"=:"false"
            , "height"=:"17.96875"
            , "horizontalTextPosition"=:"center"
            , "iconTextGap"=:"4"
            , "modelName"=:"six_pos"
            , "modelPosition"=:"shead"
            , "preferredPlacement"=:"anywhere"
            , "ratio"=:"0.5"
            , "textColor"=:"#000000"
            , "verticalTextPosition"=:"bottom"
            , "visible"=:"true"
            , "width"=:"12.9765625"
            , "x"=:"-23.13330078125"
            , "y"=:"-19.96875"
            ]
            [ txt (conf^.multiplicity.to show <> "\n")
            , qelem "PreferredPlacementDescriptor"
                [ "angle"=:"0.0"
                , "angleOffsetOnRightSide"=:"0" 
                , "angleReference"=:"absolute"
                , "angleRotationOnRightSide"=:"co"
                , "distance"=:"-1.0"
                , "frozen"=:"true"
                , "placement"=:"anywhere"
                , "side"=:"anywhere"
                , "sideReference"=:"relative_to_edge_flow"
                ]
                []
            ]
        , qelem "BendStyle" ["smoothed"=:"false"] []
        ]
    qelem name attribs childs = mkqelem (mkQName "y" name "") attribs childs

_String ::
    ( Unwrapped t ~ Text
    , Unwrapped s ~ Text
    , Functor f
    , Profunctor p
    , Rewrapped t s
    , Rewrapped s t
    )
    => p String (f String) -> p s (f t)
_String = _Wrapped . unpacked

mkKey :: ArrowXml a => [a n XmlTree] -> a n XmlTree
mkKey attributes = mkelem "key" attributes []

infixr 6 =:
(=:) :: ArrowXml a => String -> String -> a n XmlTree
k =: v = sattr k v

