effect module Middleware.Navigation
    where { command = MyCmd, subscription = MySub }
    exposing
        ( middleware
        , Location
        )

import Dom.LowLevel exposing (onWindow)
import Html exposing (Html)
import Html.Attributes
import Json.Decode as Json
import Native.Middleware.Navigation
import Process
import Program
import Program.Types exposing (Middleware, HasInnerModel)
import Task exposing (Task)


type alias HasLocationMsg programMsgs programMsg =
    { programMsgs | location : Location -> programMsg }


type alias Model =
    {}


type Msg innerMsg
    = Other innerMsg



-- MIDDLEWARE


middleware : Middleware innerModel Model innerMsg (Msg innerMsg) (HasLocationMsg programMsgs programMsg) programMsg
middleware =
    { init = Program.initNoop Other
    , update = Program.updateNoop
    , subscriptions = subscriptions
    , wrapMsg = Other
    , unwrapMsg = unwrapMsg
    , view = view
    }


unwrapMsg : Msg innerMsg -> Maybe innerMsg
unwrapMsg msg =
    case msg of
        Other innerMsg ->
            Just innerMsg


subscriptions :
    HasInnerModel Model innerModel
    -> HasLocationMsg programMsgs programMsg
    -> ( Sub msg, Sub programMsg )
subscriptions model { location } =
    ( Sub.none
    , subscription (Monitor location)
    )


view :
    HasInnerModel Model innerModel
    -> Html (Msg innerMsg)
    -> Html (Msg innerMsg)
view model innerView =
    Html.div
        [ Html.Attributes.class "navigation" ]
        [ Html.text "navigation middleware here! try changing the #hash in URL!"
        , innerView
        ]



-- LOCATION


{-| A bunch of information about the address bar.

**Note 1:** Almost everyone will want to use a URL parsing library like
[`evancz/url-parser`][parse] to turn a `Location` into something more useful
in your `update` function.

[parse]: https://github.com/evancz/url-parser

**Note 2:** These fields correspond exactly with the fields of `document.location`
as described [here](https://developer.mozilla.org/en-US/docs/Web/API/Location).

-}
type alias Location =
    { href : String
    , host : String
    , hostname : String
    , protocol : String
    , origin : String
    , port_ : String
    , pathname : String
    , search : String
    , hash : String
    , username : String
    , password : String
    }



-- EFFECT MANAGER


type MyCmd msg
    = NoOp


cmdMap : (a -> b) -> MyCmd a -> MyCmd b
cmdMap _ myCmd =
    case myCmd of
        NoOp ->
            NoOp


type MySub msg
    = Monitor (Location -> msg)


subMap : (a -> b) -> MySub a -> MySub b
subMap func (Monitor tagger) =
    Monitor (tagger >> func)



-- STATE


type alias State msg =
    { subs : List (MySub msg)
    , popWatcher : Maybe PopWatcher
    }


type PopWatcher
    = Normal Process.Id
    | InternetExplorer Process.Id Process.Id



-- INIT


init : Task Never (State msg)
init =
    Task.succeed (State [] Nothing)



-- SELF MESSAGES


onSelfMsg : Platform.Router msg Location -> Location -> State msg -> Task Never (State msg)
onSelfMsg router location state =
    notify router state.subs location
        &> Task.succeed state


(&>) task1 task2 =
    Task.andThen (\_ -> task2) task1



-- APP MESSAGES


onEffects : Platform.Router msg Location -> List (MyCmd msg) -> List (MySub msg) -> State msg -> Task Never (State msg)
onEffects router cmds subs { popWatcher } =
    let
        stepState =
            case ( subs, popWatcher ) of
                ( [], Just watcher ) ->
                    killPopWatcher watcher
                        &> Task.succeed (State subs Nothing)

                ( _ :: _, Nothing ) ->
                    Task.map (State subs << Just) (spawnPopWatcher router)

                ( _, _ ) ->
                    Task.succeed (State subs popWatcher)
    in
        Task.sequence (List.map (cmdHelp router subs) cmds)
            &> stepState


cmdHelp : Platform.Router msg Location -> List (MySub msg) -> MyCmd msg -> Task Never ()
cmdHelp router subs cmd =
    case cmd of
        NoOp ->
            Task.succeed ()


notify : Platform.Router msg Location -> List (MySub msg) -> Location -> Task x ()
notify router subs location =
    let
        send (Monitor tagger) =
            Platform.sendToApp router (tagger location)
    in
        Task.sequence (List.map send subs)
            &> Task.succeed ()



-- POP WATCHER STUFF


spawnPopWatcher : Platform.Router msg Location -> Task x PopWatcher
spawnPopWatcher router =
    let
        reportLocation _ =
            Platform.sendToSelf router (Native.Middleware.Navigation.getLocation ())
    in
        if Native.Middleware.Navigation.isInternetExplorer11 () then
            Task.map2 InternetExplorer
                (Process.spawn (onWindow "popstate" Json.value reportLocation))
                (Process.spawn (onWindow "hashchange" Json.value reportLocation))
        else
            Task.map Normal <|
                Process.spawn (onWindow "popstate" Json.value reportLocation)


killPopWatcher : PopWatcher -> Task x ()
killPopWatcher popWatcher =
    case popWatcher of
        Normal pid ->
            Process.kill pid

        InternetExplorer pid1 pid2 ->
            Process.kill pid1
                &> Process.kill pid2
