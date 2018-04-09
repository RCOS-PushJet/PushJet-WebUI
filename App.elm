import Http            exposing (..)
import Html            exposing (..)
import Html.Events     exposing (..)
import Html.Attributes exposing (..)
import Json.Decode     exposing (..)
import WebSocket
import Uuid

import Random.Pcg as Random

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
    , subscriptions = (\_ -> WebSocket.listen "ws://128.113.17.41:81/ws" NewMsg)
    }


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GenUuid seed ->
            (model, Random.generate SubUuid Uuid.uuidGenerator)
        SubUuid uuid ->
            let dev = Uuid.toString uuid in
            let prt = Http.multipartBody [ Http.stringPart "uuid"    dev
                                         , Http.stringPart "service" model.public
                                         ] in
            let req = Http.post "http://128.113.17.41:81/subscription" prt (succeed 200) in
                ({ model | uuid = dev }, Http.send SubWS req)
        SubWS   _ ->
            (model, WebSocket.send "ws://128.113.17.41:81/ws" model.uuid)
        NewMsg  jsn ->
            case messagePushJetDecoder jsn of
                Ok  msg ->
                    ({ model | messages = model.messages ++ [ msg ] }, Cmd.none)
                Err msg ->
                    (model , Cmd.none)


view : Model -> Html Msg
view model =
    div [] (List.map viewMessage model.messages)


viewMessage : MessagePushJet -> Html Msg
viewMessage msg =
    case msg of
        MessagePayload msg ->
            if msg.message.link == "" then
                div []
                    [ text (msg.message.title ++ ": " ++ msg.message.message) ]
            else
                div []
                    [ a [ href msg.message.link ] [ text (msg.message.title ++ ": " ++ msg.message.message) ] ]
-- vim:ft=haskell:
