module Middleware.Reset exposing (middleware)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Program
import Program.Types exposing (Middleware, HasInnerModel)


type Msg innerMsg
    = Other innerMsg
    | Reset


type alias Model innerModel =
    { initialModel : innerModel }


middleware : Middleware innerModel (Model innerModel) innerMsg (Msg innerMsg) programMsgs msg
middleware =
    { init = init
    , update = update
    , subscriptions = Program.subscriptionsNoop
    , wrapMsg = Other
    , unwrapMsg = unwrapMsg
    , view = view
    }


init :
    ( innerModel, Cmd innerMsg )
    -> ( HasInnerModel (Model innerModel) innerModel, Cmd (Msg innerMsg) )
init ( innerModel, innerCmd ) =
    ( { innerModel = innerModel, initialModel = innerModel }
    , Cmd.map Other innerCmd
    )


update :
    Msg innerMsg
    -> HasInnerModel (Model innerModel) innerModel
    -> programMsgs
    -> ( HasInnerModel (Model innerModel) innerModel, Cmd (Msg innerMsg), Maybe programMsg )
update msg model programMsgs =
    ( case msg of
        Reset ->
            { model | innerModel = model.initialModel }

        _ ->
            model
    , Cmd.none
    , Nothing
    )


unwrapMsg : Msg innerMsg -> Maybe innerMsg
unwrapMsg msg =
    case msg of
        Other innerMsg ->
            Just innerMsg

        _ ->
            Nothing


view :
    HasInnerModel (Model innerModel) innerModel
    -> Html (Msg innerMsg)
    -> Html (Msg innerMsg)
view model innerView =
    Html.div [ Html.Attributes.class "reset" ]
        [ Html.text <| "resetting middleware here!"
        , Html.button [ Html.Events.onClick Reset ] [ Html.text "RESET THE MODEL!" ]
        , innerView
        ]
