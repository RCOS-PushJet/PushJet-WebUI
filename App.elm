import Http            exposing (..)
import Html            exposing (..)
import Html.Events     exposing (..)
import Html.Attributes exposing (..)
import WebSocket
import Navigation

import Messages        exposing (..)

pushJetWebSocket = "wss://api.pushjet.io/ws"

type alias Model =
    { uuid     : String
    , messages : List MessagePushJet
    }

main : Program Never Model Msg
main = program
    { init =
        let uuid = "2aac6626-b783-48d1-881e-fc0eb99659d1"
            -- don't worry too much about this, it's an nginx reverse proxy which sets the
            -- Access-Control-Allow-Origin: * header
            url  = "http://ec2-34-217-64-134.us-west-2.compute.amazonaws.com/message?uuid=" ++ uuid
            req  = Http.get url messageOldDecoder in
        ({ uuid = uuid, messages = [ ] }, (Http.send OldMsg req))
    , view = view
    , update = update
    , subscriptions = (\_ -> WebSocket.listen pushJetWebSocket NewMsg)
    }

type Msg
    = OldMsg (Result Http.Error (List MessagePayloadInnerJson))
    | NewMsg String

update : Msg -> Model -> (Model, Cmd Msg)
update msg { uuid, messages } =
    let openWS = WebSocket.send pushJetWebSocket uuid in
    case msg of
        OldMsg (Ok newMessages) ->
            ({uuid = uuid, messages = messages ++ (List.map (MessagePayload << MessagePayloadJson) newMessages)}, openWS)
        OldMsg (Err _)          ->
            ({uuid = uuid, messages = messages }, openWS)
        NewMsg json ->
            case messagePushJetDecoder json of
                Ok  msg ->
                    ({uuid = uuid, messages = messages ++ [ msg ]}, Cmd.none)
                Err msg ->
                    ({uuid = uuid, messages = messages ++ [ MessageStatus (MessageOkJson msg) ]}, Cmd.none)

view : Model -> Html Msg
view model =
    div []
        [ div [] (List.map viewMessage model.messages) ]

viewMessage : MessagePushJet -> Html Msg
viewMessage msg =
    case msg of
        MessageStatus  msg ->
            div [] [ text ("status: " ++ msg.status) ]
        MessagePayload msg ->
            div []
                [ a [ href msg.message.link ] [ text (msg.message.title ++ ": " ++ msg.message.message) ] ]
-- vim:ft=haskell:
