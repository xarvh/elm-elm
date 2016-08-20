
import Compiler

import Html as H
import Html.App as App
import Html.Attributes as HA
import Html.Events as HE

import TreeDiagram

import Svg as S
import Svg.Attributes as SA


---------------

type alias Model =
    { input : String
    , errors : List String
    , output : Maybe Compiler.Node
    }




init =
    Model "" [] Nothing


type Msg
    = UserChangesInput String


update msg model =
    case msg of
        UserChangesInput input ->
            case Compiler.compile input of
                Ok tree -> Model input [] (Just tree)
                Err errors -> Model input errors Nothing





-- VIEW


(=>) prop value =
  prop (toString value)


drawLine : ( Float, Float ) -> ( Float, Float ) -> S.Svg msg
drawLine ( sourceX, sourceY ) ( targetX, targetY ) =
  S.line
    [ SA.x1 => sourceX
    , SA.y1 => sourceY
    , SA.x2 => targetX
    , SA.y2 => targetY
    , SA.stroke "black" ]
    []


drawNode : String -> S.Svg msg
drawNode value =
  S.g
    []
    [ S.circle
        [ SA.r "16", SA.stroke "black", SA.fill "white", SA.cx "0", SA.cy "0" ]
        []
    , S.text'
        [ SA.transform "translate(-8 5)" ]
        [ S.text value ]
    ]




node2tree : Compiler.Node -> TreeDiagram.Tree String
node2tree node =
    case node of
        Compiler.Node value children ->
            TreeDiagram.node value <| List.map node2tree children



view model =
    H.div
        [ HA.style
            [("text-align", "center")]
        ]
        [ H.textarea
            [ HE.onInput UserChangesInput]
            []
        , H.div
            [ HA.class "errors" ]
            (List.map H.text model.errors)
        , H.div
            [ HA.class "tree" ]
            [ case model.output of
                Just tree -> TreeDiagram.drawSvg TreeDiagram.defaultTreeLayout drawNode drawLine (node2tree tree)
                Nothing -> H.text ""
            ]
        ]



main = App.beginnerProgram
    { model = init
    , update = update
    , view = view
    }

