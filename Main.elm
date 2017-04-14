module Main exposing (..)

import Combine
import Html exposing (..)
import Html.Attributes
import Html.Events
import Parser
import Svg exposing (Svg)
import Svg.Attributes
import TreeDiagram
import TreeDiagram.Svg


type alias Model =
    String


init =
    """[ 1
, (==)
, a
, [1, (==), a, sin 1, [x, y], (2, 3), 8 * 9]
, (1, (==), a, sin 1, [x, y], (2, 3), 8 * 9)
, (a)
, sin a
, 1 * k
]
"""


update msg model =
    msg



-- VIEW


drawLine : ( Float, Float ) -> Svg msg
drawLine ( targetX, targetY ) =
    Svg.line
        [ Svg.Attributes.x1 <| toString 0
        , Svg.Attributes.y1 <| toString 0
        , Svg.Attributes.x2 <| toString targetX
        , Svg.Attributes.y2 <| toString targetY
        , Svg.Attributes.stroke "black"
        ]
        []


drawNode : ( Combine.ParseLocation, String ) -> Svg msg
drawNode ( location, label ) =
    Svg.g
        []
        [ Svg.circle
            [ Svg.Attributes.r "16"
            , Svg.Attributes.stroke "black"
            , Svg.Attributes.fill "white"
            , Svg.Attributes.cx "0"
            , Svg.Attributes.cy "0"
            , Html.Attributes.alt (toString location)
            ]
            []
        , Svg.text_
            [ Svg.Attributes.transform "translate(-8 5)" ]
            [ Svg.text label ]
        ]


{-| locatedExpressionToTree
-}
le2t : Parser.LocatedExpression -> TreeDiagram.Tree ( Combine.ParseLocation, String )
le2t (Parser.LocatedExpression location expression) =
    let
        n =
            TreeDiagram.node
    in
        case expression of
            Parser.BinaryExpression left reference right ->
                n ( location, reference ) [ le2t left, le2t right ]

            Parser.FunctionCall function arguments ->
                n (location, "call") (List.map le2t (function :: arguments))

            Parser.ListExpression elements ->
                n ( location, "[]" ) (List.map le2t elements)

            Parser.LiteralExpression value ->
                n ( location, value ) []

            Parser.TupleExpression entries ->
                n ( location, "()" ) (List.map le2t entries)

            Parser.UnaryExpression operator value ->
                n ( location, toString operator ) [ le2t value ]

            Parser.Unit ->
                n ( location, "unit" ) []

            Parser.VariableExpression variableReference ->
                n ( location, variableReference ) []


view code =
    let
        result =
            Parser.parse code
    in
        div
            [ Html.Attributes.style
                [ ( "width", "100%" )
                , ( "display", "flex" )
                , ( "flex-direction", "column" )
                , ( "align-items", "center" )
                , ( "margin-top", "1rem" )
                ]
            ]
            [ Html.textarea
                [ Html.Events.onInput identity
                , Html.Attributes.style
                    [ ( "width", "700px" )
                    , ( "height", "300px" )
                    ]
                ]
                [ text code ]
            , div
                [ Html.Attributes.style
                    [ ( "margin-top", "2rem" )
                    ]
                ]
                [ case result of
                    Err ( state, inputStream, errors ) ->
                        div
                            []
                            [ ul
                                []
                                (List.map (\e -> li [] [ text e ]) errors)
                            , div
                                []
                                [ text <| toString state ]
                            , div
                                []
                                [ text <| toString inputStream ]
                            ]

                    Ok ( _, _, tree ) ->
                      tree
                        |> le2t
                        |> TreeDiagram.Svg.draw TreeDiagram.defaultTreeLayout drawNode drawLine 
                ]
            ]


main =
    Html.beginnerProgram
        { model = init
        , update = update
        , view = view
        }
