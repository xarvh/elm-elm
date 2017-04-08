module Main exposing (..)

import Parser
import Html exposing (..)
import Html.Attributes
import Html.Events


type alias Model =
    String


init =
    "a b"


update msg model =
    msg



-- VIEW
{-
   drawLine : ( Float, Float ) -> ( Float, Float ) -> S.Svg msg
   drawLine ( sourceX, sourceY ) ( targetX, targetY ) =
     S.line
       [ SA.x1 <| toString sourceX
       , SA.y1 <| toString sourceY
       , SA.x2 <| toString targetX
       , SA.y2 <| toString targetY
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
   node2tree (Compiler.Node {value, fragmentType, children}) =
       TreeDiagram.node (value ++ ":" ++ toString fragmentType) <| List.map node2tree children
-}


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
                    [ ( "width", "700px" )
                    , ( "margin-top", "2rem" )
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
                        div [] [ text <| toString tree ]
                ]
            ]


main =
    Html.beginnerProgram
        { model = init
        , update = update
        , view = view
        }
