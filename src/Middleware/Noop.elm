module Middleware.Noop exposing (middleware)

import Html exposing (Html)
import Html.Attributes
import Program exposing (Middleware, HasNextModel)


type Msg nextMsg
    = Other nextMsg


type alias Model =
    {}


middleware : Middleware model Model msg (Msg msg)
middleware =
    { init = Program.initNoop Other
    , update = Program.updateNoop
    , subscriptions = Program.subscriptionsNoop
    , wrapMsg = Other
    , unwrapMsg = unwrapMsg
    , view = view
    }


unwrapMsg : Msg msg -> Maybe msg
unwrapMsg msg =
    case msg of
        Other innerMsg ->
            Just innerMsg


view : HasNextModel Model model -> Html (Msg msg) -> Html (Msg msg)
view model innerView =
    Html.div [ Html.Attributes.class "noop" ]
        [ Html.text "noop middleware here!"
        , innerView
        ]
