module Program
    exposing
        ( ProgramRecord
        , Middleware
        , HasNextModel
          -- noops
        , initNoop
        , subscriptionsNoop
        , updateNoop
        , viewNoop
          -- compose
        , compose2
        , compose3
        )

import Html exposing (Html)
import Program.Compose2 as C2
import Program.Compose3 as C3


type alias ProgramRecord model msg =
    { init : ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , view : model -> Html msg
    }


type alias Middleware oldModel ownModelFields oldMsg newMsg =
    { init : ( oldModel, Cmd oldMsg ) -> ( HasNextModel ownModelFields oldModel, Cmd newMsg )
    , update :
        newMsg
        -> HasNextModel ownModelFields oldModel
        -> ( HasNextModel ownModelFields oldModel, Cmd newMsg )
    , subscriptions : HasNextModel ownModelFields oldModel -> Sub newMsg
    , wrapMsg : oldMsg -> newMsg
    , unwrapMsg : newMsg -> Maybe oldMsg
    , view : HasNextModel ownModelFields oldModel -> Html newMsg -> Html newMsg
    }


type alias HasNextModel m1 m0 =
    { m1 | nextModel : m0 }



-- noops


initNoop :
    (oldMsg -> newMsg)
    -> ( model, Cmd oldMsg )
    -> ( HasNextModel {} model, Cmd newMsg )
initNoop tagger ( oldModel, oldCmd ) =
    ( { nextModel = oldModel }
    , Cmd.map tagger oldCmd
    )


subscriptionsNoop : model -> Sub msg
subscriptionsNoop model =
    Sub.none


updateNoop : msg -> model -> ( model, Cmd msg )
updateNoop msg model =
    ( model, Cmd.none )


viewNoop : model -> Html msg -> Html msg
viewNoop model innerView =
    innerView



-- compose


compose2 :
    Middleware model0 model1 msg0 msg1
    -> ProgramRecord model0 msg0
    -> Program Never (HasNextModel model1 model0) msg1
compose2 middleware program =
    Html.program
        { init = C2.init middleware program
        , subscriptions = C2.subscriptions middleware program
        , update = C2.update middleware program
        , view = C2.view middleware program
        }


compose3 :
    Middleware (HasNextModel model1 model0) model2 msg1 msg2
    -> Middleware model0 model1 msg0 msg1
    -> ProgramRecord model0 msg0
    -> Program Never (HasNextModel model2 (HasNextModel model1 model0)) msg2
compose3 middleware1 middleware2 program =
    Html.program
        { init = C3.init middleware1 middleware2 program
        , subscriptions = C3.subscriptions middleware1 middleware2 program
        , update = C3.update middleware1 middleware2 program
        , view = C3.view middleware1 middleware2 program
        }
