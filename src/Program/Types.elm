module Program.Types
    exposing
        ( Middleware
        , ProgramRecord
        , HasInnerModel
        )

import Html exposing (Html)


type alias ProgramRecord model msg programMsgs =
    { init : ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , view : model -> Html msg
    , programMsgs : programMsgs
    }


type alias Middleware innerModel thisModel innerMsg outerMsg programMsgs programMsg =
    { init :
        ( innerModel, Cmd innerMsg )
        -> ( HasInnerModel thisModel innerModel, Cmd outerMsg )
    , update :
        outerMsg
        -> HasInnerModel thisModel innerModel
        -> programMsgs
        -> ( HasInnerModel thisModel innerModel, Cmd outerMsg, Maybe programMsg )
    , subscriptions :
        HasInnerModel thisModel innerModel
        -> Sub outerMsg
    , wrapMsg : innerMsg -> outerMsg
    , unwrapMsg : outerMsg -> Maybe innerMsg
    , view :
        HasInnerModel thisModel innerModel
        -> Html outerMsg
        -> Html outerMsg
    }


type alias HasInnerModel thisModelFields innerModel =
    { thisModelFields | innerModel : innerModel }
