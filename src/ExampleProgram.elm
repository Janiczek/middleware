module ExampleProgram exposing (program)

import Html exposing (Html)
import Html.Attributes
import Middleware.Navigation exposing (Location)
import Program.Types exposing (ProgramRecord)
import Time


type alias Model =
    { counter : Int
    , locations : Int
    }


type Msg
    = Increment
    | Double
      -- called from ResetByMsg middleware:
    | LocationChanged Location


type alias ProgramMsgs =
    { location : Location -> Msg }


program : ProgramRecord Model Msg ProgramMsgs
program =
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    , programMsgs = { location = LocationChanged }
    }


init : ( Model, Cmd Msg )
init =
    ( Model 0 0, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg { counter, locations } =
    case msg of
        Increment ->
            ( Model (counter + 1) locations
            , Cmd.none
            )

        Double ->
            ( Model (counter * 2) locations
            , Cmd.none
            )

        LocationChanged location ->
            ( Model counter (locations + 1)
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every Time.second (always Increment)
        , Time.every (2 * Time.second) (always Double)
        ]


view : Model -> Html Msg
view model =
    Html.div [ Html.Attributes.class "program" ]
        [ Html.text <| "program here! model: " ++ toString model ]
