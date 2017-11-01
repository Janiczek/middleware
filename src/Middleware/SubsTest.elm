module Middleware.SubsTest exposing (middleware)

import Html exposing (Html)
import Html.Attributes
import Program exposing (Middleware, HasNextModel)
import Time


type Msg nextMsg
    = Other nextMsg
    | ToggleFlash


type alias Model =
    { isFlashing : Bool }


middleware : Middleware model Model msg (Msg msg)
middleware =
    { init = init
    , update = update
    , subscriptions = subscriptions
    , wrapMsg = Other
    , unwrapMsg = unwrapMsg
    , view = view
    }


init : ( oldModel, Cmd msg ) -> ( HasNextModel Model oldModel, Cmd (Msg msg) )
init ( oldModel, oldCmd ) =
    ( { nextModel = oldModel, isFlashing = False }
    , Cmd.map Other oldCmd
    )


update : Msg msg -> HasNextModel Model model -> ( HasNextModel Model model, Cmd (Msg msg) )
update msg model =
    case msg of
        ToggleFlash ->
            ( { model | isFlashing = not model.isFlashing }
            , Cmd.none
            )

        Other innerMsg ->
            ( model, Cmd.none )


subscriptions : HasNextModel Model model -> Sub (Msg msg)
subscriptions model =
    Time.every (500 * Time.millisecond) (always ToggleFlash)


unwrapMsg : Msg msg -> Maybe msg
unwrapMsg msg =
    case msg of
        Other innerMsg ->
            Just innerMsg

        _ ->
            Nothing


view : HasNextModel Model model -> Html (Msg msg) -> Html (Msg msg)
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
