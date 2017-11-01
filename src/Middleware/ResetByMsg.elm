module Middleware.ResetByMsg exposing (middleware)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Program
import Program.Types exposing (Middleware, HasInnerModel)


type Msg innerMsg
    = Other innerMsg
    | Reset


type alias Model =
    {}


type alias HasResetMsg programMsgs programMsg =
    { programMsgs | reset : programMsg }


middleware : Middleware innerModel Model innerMsg (Msg innerMsg) (HasResetMsg programMsgs programMsg) programMsg
middleware =
    { init = Program.initNoop Other
    , update = update
    , subscriptions = Program.subscriptionsNoop
    , wrapMsg = Other
    , unwrapMsg = unwrapMsg
    , view = view
    }


update :
    Msg innerMsg
    -> HasInnerModel Model innerModel
    -> HasResetMsg programMsgs programMsg
    -> ( HasInnerModel Model innerModel, Cmd (Msg innerMsg), Maybe programMsg )
update msg model { reset } =
    case msg of
        Reset ->
            ( model, Cmd.none, Just reset )

        _ ->
            ( model, Cmd.none, Nothing )


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
    Html.div [ Html.Attributes.class "reset" ]
        [ Html.text <| "resetting middleware here!"
        , Html.button [ Html.Events.onClick Reset ] [ Html.text "RESET THE MODEL!" ]
        , innerView
        ]
