module Main exposing (..)

import Http exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Json.Decode exposing (..)
import WebSocket
import Array
import Uuid
import Color
import Random.Pcg as Random
import Bootstrap.Text as Text
import Bootstrap.Alert as Alert
import Bootstrap.Navbar as Navbar
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Navbar as Navbar
import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Grid.Row as Row
import Bootstrap.Grid.Col as Col
import Bootstrap.Card.Block as Block
import Bootstrap.Form.Input as Input
import Bootstrap.Utilities.Spacing as Spacing
import Messages exposing (..)


type alias Model =
    { public : String
    , uuid : String
    , messages : List MessagePushJet
    , navbar : Navbar.State
    }


type Msg
    = NewMsg String
    | SavePub String
    | Submit
    | GenUuid Int
    | SubUuid Uuid.Uuid
    | SubWS (Result Http.Error Int)
    | NavbarMsg Navbar.State
    | AlertMsg MessagePushJet Alert.Visibility


webSocketEndpoint =
    "ws://128.113.17.41:81/ws"


subscribeEndpoint =
    "http://128.113.17.41:81/subscription"


main : Program Never Model Msg
main =
    program
        { init =
            let
                ( navbarState, navbarCmd ) =
                    Navbar.initialState NavbarMsg
            in
                ( { public = ""
                  , uuid = ""
                  , messages = []
                  , navbar = navbarState
                  }
                , navbarCmd
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SavePub pub ->
            ( { model | public = pub }
            , Cmd.none
            )

        Submit ->
            ( model, Random.generate GenUuid (Random.int Random.minInt Random.maxInt) )

        GenUuid seed ->
            ( model, Random.generate SubUuid Uuid.uuidGenerator )

        SubUuid uuid ->
            let
                prt =
                    Http.multipartBody
                        [ Http.stringPart "uuid" (Uuid.toString uuid)
                        , Http.stringPart "service" model.public
                        ]
            in
                ( { model | uuid = (Uuid.toString uuid) }
                , Http.send SubWS (Http.post subscribeEndpoint prt (succeed 200))
                )

        SubWS _ ->
            ( model, WebSocket.send webSocketEndpoint model.uuid )

        NewMsg jsn ->
            case messagePushJetDecoder jsn of
                Ok msg ->
                    ( { model | messages = model.messages ++ [ msg ] }, Cmd.none )

                Err msg ->
                    ( model, Cmd.none )

        NavbarMsg state ->
            ( { model | navbar = state }, Cmd.none )

        AlertMsg msg visibility ->
            let
                allMsgs =
                    List.filter (\x -> x /= msg) model.messages
            in
                -- order doesn't matter; we don't bother preserving it
                ( { model | messages = { msg | alert = visibility } :: allMsgs }
                , Cmd.none
                )


view : Model -> Html Msg
view model =
    Grid.container []
        [ CDN.stylesheet
        , Navbar.config NavbarMsg
            |> Navbar.withAnimation
            |> Navbar.fixTop
            |> Navbar.lightCustom (Color.rgb 0x47 0x64 0xAD)
            |> Navbar.brand
                [ href "#" ]
                [ div [ style [ ( "color", "#ffffff" ) ] ]
                    [ img
                        [ src "https://files.readme.io/PBWzCbL3Qn2Trac8l1Vz_pushjet_jet.png"
                        , class "d-inline-block align-top"
                        , style [ ( "width", "30px" ) ]
                        ]
                        []
                    , (text " PushJet WebUI")
                    ]
                ]
            |> Navbar.customItems
                [ Navbar.formItem []
                    [ Input.text
                        [ Input.attrs
                            [ placeholder "Service Token"
                            , onInput SavePub
                            ]
                        ]
                    , Button.button
                        [ Button.success
                        , Button.attrs
                            [ Spacing.ml2Sm
                            , onClick Submit
                            ]
                        ]
                        [ text "Go!" ]
                    ]
                ]
            |> Navbar.view model.navbar
        , div [ style [ ( "margin-top", "60px" ) ] ]
            (List.map msgToAlert model.messages)
        ]


msgToAlert msg =
    let
        children =
            let
                child1 =
                    []

                child2 =
                    if msg.message.title == "" then
                        []
                    else
                        [ Alert.h4 [] [ text msg.message.title ] ]

                child3 =
                    [ text msg.message.message ]

                child4 =
                    if msg.message.link == "" then
                        []
                    else
                        [ text " "
                        , Alert.link [ href msg.message.link ] [ text "link" ]
                        ]
            in
                child1 ++ child2 ++ child3 ++ child4

        alertType =
            case msg.message.level of
                4 ->
                    Alert.warning

                5 ->
                    Alert.danger

                _ ->
                    Alert.light
    in
        div [ class "mt-4" ]
            [ Alert.config
                |> alertType
                |> Alert.dismissable (AlertMsg msg)
                |> Alert.children children
                |> Alert.view msg.alert
            ]



-- -- vim:ft=haskell:
