import Html            exposing (..)
import Html.Events     exposing (..)
import Html.Attributes exposing (..)
import WebSocket
import Json.Decode

pushJetWebSocket = "wss://api.pushjet.io/ws"

type alias Model =
    { uuid     : String
    , messages : List MessagePushJet
    }

main : Program Never Model Msg
main = program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

init : (Model, Cmd Msg)
init =
    ({ uuid = "", messages = [ ] }, Cmd.none)

type Msg
    = NewMsg     String
    | UpdateUUID String
    | InitWS

-- some JSON payloads that we will accept
type alias MessageOkJson      = { status : String }
type alias MessagePayloadJson = { message: MessagePayloadInnerJson }
type alias MessagePayloadInnerJson =
    { title     : String
    , message   : String
    , link      : String
    }

type MessagePushJet
    = MessageOk      MessageOkJson
    | MessagePayload MessagePayloadJson

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
        Ok ok -> Ok (MessageOk ok)
        Err _ -> case Json.Decode.decodeString messagePayloadDecoder json of
            Ok ok -> Ok (MessagePayload ok)
            Err _ -> Err "Could not parse JSON"

update : Msg -> Model -> (Model, Cmd Msg)
update msg { uuid, messages } =
    case msg of
        NewMsg     json ->
            case messagePushJetDecoder json of
                Ok  msg ->
                    ({uuid = uuid, messages = messages ++ [ msg ]}, Cmd.none)
                Err msg ->
                    ({uuid = uuid, messages = messages }, Cmd.none) -- TODO
        UpdateUUID uuid ->
            ({uuid = uuid, messages = messages }, Cmd.none)
        InitWS ->
            ({uuid = uuid, messages = messages },
             (WebSocket.send pushJetWebSocket uuid))

subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen pushJetWebSocket NewMsg

view : Model -> Html Msg
view model =
    div []
        [ input  [ onInput UpdateUUID ] []
        , button [ onClick InitWS ] [text "Submit UUID"]
        , div [] (List.map viewMessage model.messages)
        ]

viewMessage : MessagePushJet -> Html Msg
viewMessage msg =
    case msg of
        MessageOk      msg -> 
            div [] [ text ("status: " ++ msg.status) ]
        MessagePayload msg ->
            div []
                [ a [ href msg.message.link ] [ text (msg.message.title ++ ": " ++ msg.message.message) ] ]
