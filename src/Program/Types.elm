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


type alias Middleware innerModel thisModel innerMsg newMsg programMsgs programMsg =
    { init :
        ( innerModel, Cmd innerMsg )
        -> ( { thisModel | innerModel : innerModel }, Cmd newMsg )
    , update :
        newMsg
        -> { thisModel | innerModel : innerModel }
        -> programMsgs
        -> ( { thisModel | innerModel : innerModel }, Cmd newMsg, Maybe programMsg )
    , subscriptions :
        { thisModel | innerModel : innerModel }
        -> Sub newMsg
    , wrapMsg : innerMsg -> newMsg
    , unwrapMsg : newMsg -> Maybe innerMsg
    , view :
        { thisModel | innerModel : innerModel }
        -> Html newMsg
        -> Html newMsg
    }


type alias HasInnerModel thisModelFields innerModel =
    { thisModelFields | innerModel : innerModel }
