import Http            exposing (..)
import Html            exposing (..)
import Html.Events     exposing (..)
import Html.Attributes exposing (..)
import WebSocket
import Navigation

import Messages        exposing (..)

-- TODO: get websockets back to original functionality
-- TODO: read uuid from query param?
--    or read device id from query param to subscribe?

pushJetWebSocket = "ws://128.113.17.41:81/ws"

type alias Model =
    { uuid     : String
    , messages : List MessagePushJet
    }

main : Program Never Model Msg
main = program
    { init =
        let uuid = "2ffa1bbb-2c73-4426-8c1f-c8e11ca861f7"
            -- don't worry too much about this, it's an nginx reverse proxy which sets the
            -- Access-Control-Allow-Origin: * header
            url = "http://128.113.17.41:81/message?uuid=" ++ uuid
            req = Http.get url messageOldDecoder in
        ({ uuid = uuid, messages = [ ] }, (Http.send OldMsg req))
    , view = view
    , update = update
    , subscriptions = (\_ -> WebSocket.listen pushJetWebSocket NewMsg)
    }

type Msg
    = OldMsg (Result Http.Error (List MessagePayloadInnerJson))
    | NewMsg String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let openWS = WebSocket.send pushJetWebSocket model.uuid in
    case msg of
        OldMsg (Ok newMessages) ->
            ({ model | messages = model.messages ++ (List.map (MessagePayload << MessagePayloadJson) newMessages)}, openWS)
        OldMsg (Err _)          ->
            (model, openWS)
        NewMsg json ->
            case messagePushJetDecoder json of
                Ok  msg ->
                    ({ model | messages = model.messages ++ [ msg ]}, Cmd.none)
                Err msg ->
                    (model, Cmd.none)


view : Model -> Html Msg
view model =
    div []
        [ div [] (List.filterMap viewMessage model.messages) ]
viewMessage : MessagePushJet -> Maybe (Html Msg)
viewMessage msg =
    case msg of
        MessageStatus  msg ->
            Nothing
        MessagePayload msg ->
            Just (
                div []
                    [ a [ href msg.message.link ] [ text (msg.message.title ++ ": " ++ msg.message.message) ] ])
-- vim:ft=haskell:
