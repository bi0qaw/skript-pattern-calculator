import Browser
import Html exposing (..)
import Html.Events exposing (onInput)
import Html.Attributes exposing (placeholder, title, value, cols, rows, name, readonly, class)
import Set exposing (Set)
import Parser as P exposing (Parser, (|.), (|=))

main =
  Browser.element 
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions 
    }


-- Pattern

type Pattern
  = Empty
  | WS
  | Literal String
  | And Pattern Pattern
  | Optional Pattern
  | Grouped Pattern
  | Or Pattern Pattern
  | Expression String
  | Regex Pattern


toString : Pattern -> String
toString pattern =
  case pattern of
    Empty ->
      ""

    WS ->
      " "
    
    Literal str ->
      str

    And left right ->
      (toString left) ++ (toString right)

    Optional pat ->
      "[" ++ (toString pat) ++ "]"

    Grouped pat ->
      "(" ++ (toString pat) ++ ")"

    Or left right ->
      (toString left) ++ "|" ++ (toString right)

    Expression str ->
      "%" ++ str ++ "%"

    Regex pat ->
      "<" ++ (toString pat) ++ ">"


toMatchedString : Pattern -> String
toMatchedString pattern =
  case pattern of
    Empty ->
      ""

    WS ->
      " "
    
    Literal str ->
      str

    And left right ->
      (toMatchedString left) ++ (toMatchedString right)

    Optional pat ->
      toMatchedString pat

    Grouped pat ->
      toMatchedString pat

    Or _ _ ->
      "UNEXPECTED OR!"

    Expression str ->
      "%" ++ str ++ "%"

    Regex pat ->
      "<" ++ (toMatchedString pat) ++ ">"


condense : Pattern -> Pattern
condense pattern =
  case pattern of
    Empty ->
      Empty

    WS ->
      Empty
    
    Literal _ ->
      pattern

    And left right ->
      case ((condense left), (condense right)) of
        (Empty, Empty) ->
          Empty
        
        (l, Empty) ->
          l

        (Empty, r) ->
          r

        (l, r) ->
          And l r

    Optional pat ->
      case condense pat of
        Empty ->
          Empty

        p ->
          Optional p

    Grouped pat ->
      case condense pat of
        Empty ->
          Empty

        p ->
          Grouped p

    Or left right ->
      case ((condense left), (condense right)) of
        (Empty, Empty) ->
          Empty

        (l, Empty) ->
          l

        (Empty, r) ->
          r

        (l, r) ->
          Or l r

    Expression _ ->
      pattern

    Regex pat ->
      case condense pat of
        Empty ->
          Empty
        
        p ->
          Regex p


combinations : Pattern -> List Pattern
combinations pattern =
  case pattern of
    Empty ->
      []

    WS ->
      [WS]

    Literal _ ->
      [pattern]

    And left right ->
      andCombinations (combinations left) (combinations right)

    Optional pat ->
      Empty :: (combinations pat)
    
    Grouped pat ->
      List.map Grouped (combinations pat)

    Or left right ->
      List.append (combinations left) (combinations right)

    Expression _ ->
      [pattern]

    Regex pat ->
      List.map Regex (combinations pat)


andCombinations : List Pattern -> List Pattern -> List Pattern
andCombinations lefts rights =
  case lefts of
    [] ->
      []
    l :: ls ->
      List.append 
        (List.map (\r -> And l r) rights)
        (andCombinations ls rights)


toList : Pattern -> List Pattern
toList pattern =
  case pattern of
    Empty ->
      []

    And left right ->
      List.append (toList left) (toList right)

    _ ->
      [pattern]


untilFirstRequired : Pattern -> Pattern
untilFirstRequired pattern =
  reduceAndSequence (untilFirstHelper (toList pattern))


untilFirstHelper : List Pattern -> List Pattern
untilFirstHelper patterns =
  case patterns of
    [] -> 
      []

    p :: ps ->
      case p of
        Empty ->
          untilFirstHelper ps
        
        WS ->
          p :: (untilFirstHelper ps)

        Optional _ ->
          p :: (untilFirstHelper ps)

        _ ->
          [p]


-- Parser

whitespaceChars : Set Char
whitespaceChars =
  Set.fromList [' ', '\r', '\t', '\n']


isWhitespace : Char -> Bool
isWhitespace char =
  Set.member char whitespaceChars


whitespace : Parser Pattern
whitespace =
  P.map (\_ -> WS) (
    P.succeed ()
      |. P.chompIf isWhitespace
      |. P.chompWhile isWhitespace
  )


specialChars : Set Char
specialChars =
  let
    reserved = Set.fromList ['(', ')', '[', ']', '|', '%', '<', '>']
  in 
    Set.union reserved whitespaceChars


isSpecial : Char -> Bool
isSpecial char =
  Set.member char specialChars


isLiteral : Char -> Bool
isLiteral char =
  not (isSpecial char)


literal : Parser Pattern
literal =
  P.map Literal (
    P.getChompedString <|
      P.succeed ()
        |. P.chompIf isLiteral
        |. P.chompWhile isLiteral
  )


single : Parser Pattern
single =
  P.oneOf
    [ whitespace
    , literal
    , optional
    , grouped
    , expression
    , regex
    ]


andSequence : Parser (List Pattern)
andSequence = 
  P.loop [] andSequenceHelp


andSequenceHelp : List Pattern -> Parser (P.Step (List Pattern) (List Pattern))
andSequenceHelp patterns =
  P.oneOf 
    [ P.succeed (\p -> P.Loop (p :: patterns))
        |= single
    , P.succeed ()
        |> P.map (\_ -> P.Done (List.reverse patterns))
    ]


reduceAndSequence : List Pattern -> Pattern
reduceAndSequence patterns =
  case patterns of
    [] ->
      Empty

    p :: [] ->
      p

    p :: ps ->
      And p (reduceAndSequence ps)


and : Parser Pattern
and =
  P.map reduceAndSequence andSequence


optional : Parser Pattern
optional =
  P.succeed Optional
    |. P.symbol "["
    |= nested
    |. P.symbol "]"


grouped : Parser Pattern
grouped =
  P.succeed Grouped
    |. P.symbol "("
    |= nested
    |. P.symbol ")"


orSequence : Parser (List Pattern)
orSequence =
  P.sequence 
    { start = ""
    , separator = "|"
    , end = ""
    , spaces = P.succeed ()
    , item = and
    , trailing = P.Forbidden
    }


reduceOrSequence : List Pattern -> Pattern
reduceOrSequence patterns =
  case patterns of 
    [] ->
      Empty

    p :: [] ->
      p

    p :: ps ->
      Or p (reduceOrSequence ps)


or : Parser Pattern
or = 
  P.map reduceOrSequence orSequence


expression : Parser Pattern
expression =
  P.succeed Expression
    |. P.symbol "%"
    |= (P.getChompedString <| P.chompWhile (\c -> not (c == '%')))
    |. P.symbol "%"


regex : Parser Pattern
regex =
  P.succeed Regex
    |. P.symbol "<"
    |= nested
    |. P.symbol ">"


nested : Parser Pattern
nested =
  or


parser : Parser Pattern
parser =
  P.succeed identity
    |= nested
    |. P.end


parse : String -> Pattern
parse str =
  case P.run parser str of
    Ok pat ->
      pat
    
    Err _ ->
      Empty


parseString : String -> String
parseString str =
  case P.run parser str of
    Ok pat ->
      toString pat

    Err _ ->
      "error"

  
allCombinations : String -> List String
allCombinations str =
  mapToString (combinations (parse str))


mapToString : List Pattern -> List String
mapToString patterns =
  List.map toMatchedString patterns


-- MODEL

type alias Model 
  = { pattern: String
    , count : Int
    , countUntilRequired : Int
    , combinations: List String
    , untilRequiredCombinations: List String
    , limit : Int
    , show : String
    }


emptyModel : Model
emptyModel =
  { pattern = ""
  , count = 0
  , countUntilRequired = 0
  , combinations = []
  , untilRequiredCombinations = []
  , limit = 100
  , show = "all"
  }


initPattern : String
initPattern = "[(all [[of] the]|the)] permissions (from|of) %players%"


init : () -> (Model, Cmd Msg)
init _ =
  ( updateModel emptyModel initPattern
  , Cmd.none
  )


-- UPDATE

type Msg 
  = PatternChange String
  | LimitChange String
  | ChangeShow String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    PatternChange str ->
      ( updateModel model str
      , Cmd.none
      )

    LimitChange str ->
      if ((String.length str) == 0) then
        (
          { model 
          | limit = 0
          }
          , Cmd.none
        )
      else
        case String.toInt str of
          Just limit ->
            (
              { model 
              | limit = limit
              }
            , Cmd.none
            )

          Nothing ->
            ( model
            , Cmd.none
            )

    ChangeShow show ->
      ( 
        { model 
        | show = show}
      , Cmd.none
      )


updateModel : Model -> String -> Model
updateModel model pattern =
  let
    pat = parse pattern
    combs = mapToString (combinations pat)
    untilRequiredCombs = mapToString (combinations (untilFirstRequired pat))
  in
    { model 
    | pattern = pattern
    , count = List.length combs
    , countUntilRequired = List.length untilRequiredCombs
    , combinations = combs
    , untilRequiredCombinations = untilRequiredCombs
    }


-- VIEW

view : Model -> Html Msg
view model =
  div [ class "main" ]
    [ text "Pattern"
    , div [ class "pattern_input" ] 
      [ textarea 
        [ placeholder "pattern"
        , cols textareaCols
        , rows (patternInputRows model)
        , value model.pattern
        , onInput PatternChange 
        ]
        []
      ]
    , div [ title showTitle , class "show" ] 
      [ text "Show: "
      , select [ onInput ChangeShow ] 
          [ option [ value "all" ] [ text "All" ]
          , option [ value "until_first_required" ] [ text "Until First Required" ]
          ]
      ]
    , div [ class "output_limit" ]
        [ text "Limit output to: " 
        , input [ placeholder "limit", value (String.fromInt model.limit), onInput LimitChange ] []
        ]
    , hr [] []
    , div [ class "all_combinations_count" ] [ text ("All Combinations: " ++ (String.fromInt model.count)) ]
    , div [ class "until_first_required_count" ] [ text ("Until First Required Combinations: " ++ (String.fromInt model.countUntilRequired)) ]
    , hr [] []
    , combinationsHtml model
    ]


showTitle : String
showTitle =
  List.foldr (++) "" 
    [ "'All' shows all possible combinations.\n"
    , "'Until First Required' shows all possible beginnings of the pattern "
    , "until it finds a required element."
    ]


textareaCols : Int
textareaCols = 80


limitExceedText : Model -> String
limitExceedText model =
  "Only showing first " ++ (String.fromInt model.limit) ++ " combinations!" 


combinationsHtml : Model -> Html Msg
combinationsHtml model =
  let 
    patterns = 
      case model.show of 
        "all" ->
          model.combinations
        
        "until_first_required" ->
          model.untilRequiredCombinations

        _ ->
          []
    count = List.length patterns
    lines = 
      if (count > model.limit) then
        List.intersperse "\n" (List.take model.limit patterns)
      else
        List.intersperse "\n" patterns
    str = List.foldr (++) "" lines
    
  in
    div [ class "combinations" ] [ code [] (List.map (\p -> text p) lines) ]


patternInputRows : Model -> Int
patternInputRows model =
  let 
    compare = 
      \c ->
        if (c == '\n') then 1
        else 0
  in
    1 + (List.sum (List.map compare (String.toList model.pattern)))


inputSize : Model -> Int
inputSize model =
  max 20 (String.length model.pattern)


-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none