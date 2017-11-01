module Middleware.SubsTest exposing (middleware)

import Html exposing (Html)
import Html.Attributes
import Program.Types exposing (Middleware, HasInnerModel)
import Time


type Msg innerMsg
    = Other innerMsg
    | ToggleFlash


type alias Model =
    { isFlashing : Bool }


middleware : Middleware innerModel Model innerMsg (Msg innerMsg) programMsgs programMsg
middleware =
    { init = init
    , update = update
    , subscriptions = subscriptions
    , wrapMsg = Other
    , unwrapMsg = unwrapMsg
    , view = view
    }


init :
    ( innerModel, Cmd innerMsg )
    -> ( HasInnerModel Model innerModel, Cmd (Msg innerMsg) )
init ( innerModel, innerCmd ) =
    ( { innerModel = innerModel, isFlashing = False }
    , Cmd.map Other innerCmd
    )


update :
    Msg innerMsg
    -> HasInnerModel Model innerModel
    -> programMsgs
    -> ( HasInnerModel Model innerModel, Cmd (Msg innerMsg), Maybe programMsg )
update msg model programMsgs =
    case msg of
        ToggleFlash ->
            ( { model | isFlashing = not model.isFlashing }
            , Cmd.none
            , Nothing
            )

        Other innerMsg ->
            ( model, Cmd.none, Nothing )


subscriptions :
    HasInnerModel Model innerModel
    -> Sub (Msg innerMsg)
subscriptions model =
    Time.every (500 * Time.millisecond) (always ToggleFlash)


unwrapMsg : Msg innerMsg -> Maybe innerMsg
unwrapMsg msg =
    case msg of
        Other innerMsg ->
            Just innerMsg

        _ ->
            Nothing


view :
    HasInnerModel Model innerModel
    -> Html (Msg innerMsg)
    -> Html (Msg innerMsg)
view model innerView =
    Html.div
        [ Html.Attributes.classList
            [ ( "sub", True )
            , ( "is-flashing", model.isFlashing )
            ]
        ]
        [ Html.text <|
            "sub middleware here!"
                ++ (if model.isFlashing then
                        " I'm flashing!"
                    else
                        ""
                   )
        , innerView
        ]
