import Http            exposing (..)
import Html            exposing (..)
import Html.Events     exposing (..)
import Html.Attributes exposing (..)
import Json.Decode     exposing (..)
import WebSocket
import Array
import Uuid

import Random.Pcg       as Random
import Bootstrap.CDN    as CDN
import Bootstrap.Grid   as Grid
import Bootstrap.Navbar as Navbar
import Bootstrap.Button as Button
import Bootstrap.Card   as Card
import Bootstrap.Grid.Row as Row
import Bootstrap.Grid.Col as Col
import Bootstrap.Card.Block as Block

import Messages        exposing (..)


type alias Model =
    { public   : String
    , uuid     : String
    , messages : List MessagePushJet
    }


type Msg =
    NewMsg  String
  | GenUuid Int
  | SubUuid Uuid.Uuid
  | SubWS   (Result Http.Error Int)
 

webSocketEndpoint = "ws://128.113.17.41:81/ws"
subscribeEndpoint = "http://128.113.17.41:81/subscription"


main : Program Never Model Msg
main = program
    { init =
        ( { public   = "b633-aa1685-129513384c94-26893-06d1766f1",
            uuid     = "",
            messages = [ ] },
          Random.generate GenUuid (Random.int Random.minInt Random.maxInt)
        )
    , view = view
    , update = update
    , subscriptions = (\_ -> WebSocket.listen webSocketEndpoint NewMsg)
    }


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
        SubWS   _ ->
            ( model, WebSocket.send webSocketEndpoint model.uuid )
        NewMsg  jsn ->
            case messagePushJetDecoder jsn of
                Ok  msg ->
                    ( { model | messages = model.messages ++ [ msg ] }, Cmd.none )
                Err msg ->
                    ( model , Cmd.none )


view : Model -> Html Msg
view model =
    Grid.container []
        [ CDN.stylesheet
        , div [] (List.map viewMessage model.messages)
        ]


viewMessage : MessagePushJet -> Html Msg
viewMessage msg =
    case msg of
        MessagePayload msg ->
            Card.config ((cardStyle msg.message.level) ++ [ Card.attrs [ class "mt-4" ] ])
                |> Card.block [] (cardBlockContents msg.message)
                |> Card.view


cardBlockContents msg =
    let block1 = [ ] in
    let block2 = if msg.title == "" then block1 else (cardTitle msg.title) :: block1 in
    let block3 = (Block.text [] [ text msg.message ]) :: block2                      in
    let block4 = if msg.link  == "" then block3 else (cardLink msg.link)   :: block3 in
    block4


cardTitle t = Block.titleH5 [] [ text t ]
cardLink  l = Block.custom <| Button.button [ Button.attrs [ href l ] ] [ text "link" ]
cardStyle x =
    case x of
        4 -> [ Card.warning ]
        5 -> [ Card.danger  ]
        _ -> [ ]
-- vim:ft=haskell:
