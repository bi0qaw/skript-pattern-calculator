module Main exposing (Model, Msg(..))

import Browser
import Element as E exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import PatternParser exposing (Pattern, combinations, mapToString, parse, untilFirstRequired)


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { pattern : String
    , parsed : Maybe Pattern
    , allCount : Int
    , prefixCount : Int
    , allCombinations : List String
    , prefixCombinations : List String
    , limit : Int
    , prefixOnly : Bool
    }


emptyModel : Model
emptyModel =
    { pattern = ""
    , parsed = Nothing
    , allCount = 0
    , prefixCount = 0
    , allCombinations = []
    , prefixCombinations = []
    , limit = 100
    , prefixOnly = False
    }


initPattern : String
initPattern =
    "[(all [[of] the]|the)] permissions (from|of) %players%"


init : () -> ( Model, Cmd Msg )
init _ =
    ( updateModel emptyModel initPattern
    , Cmd.none
    )



-- UPDATE


type Msg
    = PatternChange String
    | LimitChange String
    | ChangeShow Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PatternChange str ->
            ( updateModel model str
            , Cmd.none
            )

        LimitChange str ->
            if String.length str == 0 then
                ( { model
                    | limit = 0
                  }
                , Cmd.none
                )

            else
                case String.toInt str of
                    Just limit ->
                        ( { model
                            | limit = limit
                          }
                        , Cmd.none
                        )

                    Nothing ->
                        ( model
                        , Cmd.none
                        )

        ChangeShow prefixOnly ->
            ( { model
                | prefixOnly = prefixOnly
              }
            , Cmd.none
            )


updateModel : Model -> String -> Model
updateModel model pattern =
    let
        parsed =
            case parse pattern of
                Ok parsedPattern ->
                    Just parsedPattern

                Err _ ->
                    Nothing

        combs =
            case parsed of
                Just pat ->
                    mapToString (combinations pat)

                Nothing ->
                    []

        untilRequiredCombs =
            case parsed of
                Just pat ->
                    mapToString (combinations (untilFirstRequired pat))

                Nothing ->
                    []
    in
    { model
        | pattern = pattern
        , parsed = parsed
        , allCount = List.length combs
        , prefixCount = List.length untilRequiredCombs
        , allCombinations = combs
        , prefixCombinations = untilRequiredCombs
    }



-- VIEW


view : Model -> Html Msg
view model =
    E.layout
        []
        (E.column
            [ E.centerX
            , E.width (E.maximum 800 E.fill)
            , E.spacing 30
            , E.padding 20
            ]
            [ title
            , settings model
            , results model
            ]
        )


title : Element Msg
title =
    E.row
        [ E.width E.fill
        , E.padding 50
        , Font.size 48
        , Font.family
            [ Font.typeface "Helvetica"
            ]
        , Font.regular
        , Font.color (E.rgb 0.4 0.4 0.3)
        ]
        [ E.el [ E.centerX ] (E.text "Pattern Calculator") ]


patternInput : Model -> Element Msg
patternInput model =
    let
        color =
            case model.parsed of
                Just _ ->
                    E.rgb 1 1 1

                Nothing ->
                    E.rgb 1 0.5 0.5
    in
    E.row [ E.width E.fill ]
        [ Input.multiline
            [ Background.color color
            , E.scrollbarX
            ]
            { label =
                Input.labelHidden "Pattern"
            , onChange =
                PatternChange
            , placeholder =
                Just (Input.placeholder [] (E.text "pattern"))
            , spellcheck =
                False
            , text =
                model.pattern
            }
        ]


settings : Model -> Element Msg
settings model =
    E.column
        [ E.width E.fill
        , E.spacing 20
        ]
        [ patternInput model
        , limitOutput model
        , outputType model
        ]


settingLabel : String -> Element Msg
settingLabel name =
    E.el
        [ E.width (E.px 150) ]
        (E.text name)


labelElement : String -> Element Msg
labelElement name =
    E.el
        [ E.width (E.px 200) ]
        (E.text name)


limitOutput : Model -> Element Msg
limitOutput model =
    let
        labelName =
            "Output Limit"
    in
    E.row
        []
        [ settingLabel labelName
        , Input.text []
            { label =
                Input.labelHidden labelName
            , onChange =
                LimitChange
            , text =
                String.fromInt model.limit
            , placeholder =
                Just (Input.placeholder [] (E.text "limit"))
            }
        ]


outputType : Model -> Element Msg
outputType model =
    let
        labelName =
            "Prefix Only"
    in
    E.row []
        [ settingLabel labelName
        , Input.checkbox []
            { label =
                Input.labelHidden labelName
            , onChange =
                ChangeShow
            , icon =
                Input.defaultCheckbox
            , checked =
                model.prefixOnly
            }
        ]


results : Model -> Element Msg
results model =
    let
        color =
            E.rgb 0.95 0.95 0.95

        rounded =
            3

        width =
            1

        padding =
            20
    in
    E.column
        [ E.width E.fill
        , E.spacing 20
        ]
        [ E.column
            [ E.width E.fill
            , E.spacing 10
            , E.padding padding
            , Background.color color
            , Border.color color
            , Border.rounded rounded
            , Border.width width
            ]
            [ allCount model
            , prefixCount model
            ]
        , E.el
            [ E.width E.fill
            , E.padding padding
            , Background.color color
            , Border.color color
            , Border.rounded rounded
            , Border.width width
            ]
            (combinationsOutput model)
        ]


allCount : Model -> Element Msg
allCount model =
    E.row []
        [ labelElement "All Combinations"
        , E.text (String.fromInt model.allCount)
        ]


prefixCount : Model -> Element Msg
prefixCount model =
    E.row []
        [ labelElement "Prefix Combinations"
        , E.text (String.fromInt model.prefixCount)
        ]


combinationsCount : Model -> Element Msg
combinationsCount model =
    E.row [ E.spacing 20 ]
        [ E.column []
            [ E.text "All Combinations"
            , E.text "Prefix Combinations"
            ]
        , E.column [ E.width E.shrink ]
            [ E.text (String.fromInt model.allCount)
            , E.text (String.fromInt model.prefixCount)
            ]
        ]


combinationsOutput : Model -> Element Msg
combinationsOutput model =
    let
        patterns =
            case model.prefixOnly of
                False ->
                    model.allCombinations

                True ->
                    model.prefixCombinations

        count =
            List.length patterns

        lines =
            if count > model.limit then
                List.intersperse "\n" (List.take model.limit patterns)

            else
                List.intersperse "\n" patterns

        str =
            List.foldr (++) "" lines
    in
    E.row
        [ E.width E.fill
        , Font.family
            [ Font.monospace ]
        , E.scrollbarX
        ]
        [ E.text str ]



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
