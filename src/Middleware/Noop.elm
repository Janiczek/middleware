module Middleware.Noop exposing (middleware)

import Html exposing (Html)
import Html.Attributes
import Program
import Program.Types exposing (Middleware)


type Msg innerMsg
    = Other innerMsg


type alias Model =
    {}


middleware : Middleware innerModel Model innerMsg (Msg innerMsg) programMsgs programMsg
middleware =
    { init = Program.initNoop Other
    , update = Program.updateNoop
    , subscriptions = Program.subscriptionsNoop
    , wrapMsg = Other
    , unwrapMsg = unwrapMsg
    , view = view
    }


unwrapMsg : Msg innerMsg -> Maybe innerMsg
unwrapMsg msg =
    case msg of
        Other innerMsg ->
            Just innerMsg


view : { innerModel : innerModel } -> Html (Msg innerMsg) -> Html (Msg innerMsg)
view model innerView =
    Html.div [ Html.Attributes.class "noop" ]
        [ Html.text "noop middleware here!"
        , innerView
        ]
