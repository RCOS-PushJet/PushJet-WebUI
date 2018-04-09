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


type alias Model =
    { uuid     : String
    , messages : List MessagePushJet
    }


main : Program Never Model Msg
main = program
    { init =
        let uuid = "6be07c15-94ad-4d48-bcc5-09e2359d4ebe"
            -- don't worry too much about this, it's an nginx reverse proxy which sets the
            -- Access-Control-Allow-Origin: * header
            url = "http://128.113.17.41:81/message?uuid=" ++ uuid
            req = Http.get url messageOldDecoder in
        ({ uuid = uuid, messages = [ ] }, WebSocket.send "ws://128.113.17.41:81/ws" uuid)
    , view = view
    , update = update
    , subscriptions = (\_ -> WebSocket.listen "ws://128.113.17.41:81/ws" NewMsg)
    }


type Msg
    = NewMsg String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let openWS = WebSocket.send "ws://128.113.17.41:81/ws" model.uuid in
    case msg of
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
            Just (div []
                    [ a [ href msg.message.link ] [ text (msg.message.title ++ ": " ++ msg.message.message) ] ])
-- vim:ft=haskell:
