module Program
    exposing
        ( initNoop
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
import Program.Types exposing (..)


-- noops


initNoop :
    (innerMsg -> outerMsg)
    -> ( innerModel, Cmd innerMsg )
    -> ( HasInnerModel {} innerModel, Cmd outerMsg )
initNoop tagger ( innerModel, innerCmd ) =
    ( { innerModel = innerModel }
    , Cmd.map tagger innerCmd
    )


subscriptionsNoop : model -> programMsgs -> ( Sub msg, Sub programMsg )
subscriptionsNoop model programMsgs =
    ( Sub.none, Sub.none )


updateNoop :
    msg
    -> model
    -> programMsgs
    -> ( model, Cmd msg, Maybe programMsg )
updateNoop msg model programMsgs =
    ( model, Cmd.none, Nothing )


viewNoop : model -> Html msg -> Html msg
viewNoop model innerView =
    innerView



-- compose


compose2 :
    Middleware modelProgram modelMiddleware msgProgram msgMiddleware programMsgs msgProgram
    -> ProgramRecord modelProgram msgProgram programMsgs
    -> Program Never (HasInnerModel modelMiddleware modelProgram) msgMiddleware
compose2 middleware program =
    Html.program
        { init = C2.init middleware program
        , subscriptions = C2.subscriptions middleware program
        , update = C2.update middleware program
        , view = C2.view middleware program
        }


compose3 :
    Middleware (HasInnerModel modelIn modelProgram) modelOut msgIn msgOut programMsgs msgProgram
    -> Middleware modelProgram modelIn msgProgram msgIn programMsgs msgProgram
    -> ProgramRecord modelProgram msgProgram programMsgs
    -> Program Never (HasInnerModel modelOut (HasInnerModel modelIn modelProgram)) msgOut
compose3 middlewareOut middlewareIn program =
    Html.program
        { init = C3.init middlewareOut middlewareIn program
        , subscriptions = C3.subscriptions middlewareOut middlewareIn program
        , update = C3.update middlewareOut middlewareIn program
        , view = C3.view middlewareOut middlewareIn program
        }
