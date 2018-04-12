import Http            exposing (..)
import Html            exposing (..)
import Html.Events     exposing (..)
import Html.Attributes exposing (..)
import Json.Decode     exposing (..)
import WebSocket
import Array
import Uuid

import Random.Pcg       as Random
import Bootstrap.Alert  as Alert
import Bootstrap.Navbar as Navbar
import Bootstrap.CDN    as CDN
import Bootstrap.Grid   as Grid
import Bootstrap.Navbar as Navbar
import Bootstrap.Button as Button
import Bootstrap.Card   as Card
import Bootstrap.Grid.Row as Row
import Bootstrap.Grid.Col as Col
import Bootstrap.Card.Block as Block
import Bootstrap.Form.Input as Input

import Messages        exposing (..)


type alias Model =
    { public   : String
    , uuid     : String
    , messages : List MessagePushJet
    , navbar   : Navbar.State
    }


type Msg =
    NewMsg    String
  | GenUuid   Int
  | SubUuid   Uuid.Uuid
  | SubWS     (Result Http.Error Int)
  | NavbarMsg Navbar.State


webSocketEndpoint = "ws://128.113.17.41:81/ws"
subscribeEndpoint = "http://128.113.17.41:81/subscription"


main : Program Never Model Msg
main = program
    { init =
        let (navbarState, navbarCmd) = Navbar.initialState NavbarMsg in
        ( { public   = "b633-aa1685-129513384c94-26893-06d1766f1"
          , uuid     = ""
          , messages = [ ]
          , navbar   = navbarState },
          Cmd.batch
            [ Random.generate GenUuid (Random.int Random.minInt Random.maxInt)
            , navbarCmd
            ]
        )
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Navbar.subscriptions model.navbar NavbarMsg
        , WebSocket.listen webSocketEndpoint NewMsg
        ]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GenUuid seed ->
            ( model, Random.generate SubUuid Uuid.uuidGenerator )
        SubUuid uuid ->
            let prt = Http.multipartBody
                        [ Http.stringPart "uuid"    (Uuid.toString uuid)
                        , Http.stringPart "service" model.public
                        ] in
                ( { model | uuid = (Uuid.toString uuid) },
                  Http.send SubWS (Http.post subscribeEndpoint prt (succeed 200))
                )
        SubWS _ ->
            ( model, WebSocket.send webSocketEndpoint model.uuid )
        NewMsg jsn ->
            case messagePushJetDecoder jsn of
                Ok  msg ->
                    ( { model | messages = model.messages ++ [ msg ] }, Cmd.none )
                Err msg ->
                    ( model , Cmd.none )
        NavbarMsg state ->
            ( { model | navbar = state }, Cmd.none )


view : Model -> Html Msg
view model =
    Grid.container []
        [ CDN.stylesheet
        , Navbar.config NavbarMsg
            |> Navbar.withAnimation
            |> Navbar.collapseMedium
            |> Navbar.info
            |> Navbar.brand
                 [ href "#" ]
                 [ text "PushJet" ]
            |> Navbar.view model.navbar
        , div [] (List.map msgToAlert model.messages)
        ]

msgToAlert msg =
    case msg of
        MessagePayload msg ->
            Alert.simpleInfo []
                [ Alert.h4 [] [ text msg.message.title ]
                , text msg.message.message
                , Alert.link [ href msg.message.link ] [ text "link" ]
                ]

-- -- vim:ft=haskell:
