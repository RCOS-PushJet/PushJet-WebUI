import Http            exposing (..)
import Html            exposing (..)
import Html.Events     exposing (..)
import Html.Attributes exposing (..)
import WebSocket
import Json.Decode

-- pushJetWebSocket = "wss://ec2-34-217-64-134.us-west-2.compute.amazonaws.com/ws"
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
    , subscriptions = subscriptions
    }

-- some JSON payloads that we will accept
type alias MessageOkJson      = { status : String }
type alias MessagePayloadJson = { message: MessagePayloadInnerJson }
type alias MessagePayloadInnerJson =
    { title     : String
    , message   : String
    , link      : String
    }

type MessagePushJet
    = MessageStatus  MessageOkJson
    | MessagePayload MessagePayloadJson

messageOldDecoder : Json.Decode.Decoder (List MessagePayloadInnerJson)
messageOldDecoder =
        (Json.Decode.field "messages" (Json.Decode.list messagePayloadInnerDecoder))

messageOKDecoder      =
    Json.Decode.map  MessageOkJson      (Json.Decode.field "status"  Json.Decode.string)
messagePayloadDecoder =
    Json.Decode.map  MessagePayloadJson (Json.Decode.field "message" messagePayloadInnerDecoder)
messagePayloadInnerDecoder =
    Json.Decode.map3 MessagePayloadInnerJson (Json.Decode.field "title"   Json.Decode.string)
                                             (Json.Decode.field "message" Json.Decode.string)
                                             (Json.Decode.field "link"    Json.Decode.string)

messagePushJetDecoder json =
    case Json.Decode.decodeString messageOKDecoder json of
        Ok ok -> Ok (MessageStatus ok)
        Err _ -> case Json.Decode.decodeString messagePayloadDecoder json of
            Ok ok -> Ok (MessagePayload ok)
            Err _ -> Err "Could not parse JSON"
-- json decoding end

type Msg
    = OldMsg (Result Http.Error (List MessagePayloadInnerJson))
    | NewMsg String

update : Msg -> Model -> (Model, Cmd Msg)
update msg { uuid, messages } =
    let openWS = WebSocket.send pushJetWebSocket uuid in
    case msg of
        -- handle old fetched messages and send our uuid to the websocket
        OldMsg (Ok newMessages) ->
            ({uuid = uuid, messages = messages ++ (List.map (\x -> MessagePayload (MessagePayloadJson x)) newMessages)}, openWS)
        OldMsg (Err _)          ->
            ({uuid = uuid, messages = messages},                                                                         openWS)
        -- handle new messages that come from the web socket
        NewMsg json ->
            case messagePushJetDecoder json of
                Ok  msg ->
                    ({uuid = uuid, messages = messages ++ [ msg ]}, Cmd.none)
                Err msg ->
                    ({uuid = uuid, messages = messages ++ [ MessageStatus (MessageOkJson msg) ]}, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen pushJetWebSocket NewMsg

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
