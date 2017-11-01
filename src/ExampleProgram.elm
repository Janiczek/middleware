module ExampleProgram exposing (program)

import Html exposing (Html)
import Html.Attributes
import Program.Types exposing (ProgramRecord)
import Time


type alias Model =
    Int


type Msg
    = Increment
    | Double
      -- called from ResetByMsg middleware:
    | Reset


program : ProgramRecord Model Msg { reset : Msg }
program =
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    , programMsgs = { reset = Reset }
    }


init : ( Model, Cmd Msg )
init =
    ( 0, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( (model + 1), Cmd.none )

        Double ->
            ( (model * 2), Cmd.none )

        Reset ->
            ( 0, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every Time.second (always Increment)
        , Time.every (2 * Time.second) (always Double)
        ]


view : Model -> Html Msg
view model =
    Html.div [ Html.Attributes.class "program" ]
        [ Html.text <| "program here! counter: " ++ toString model ]
