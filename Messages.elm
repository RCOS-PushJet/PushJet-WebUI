module Messages exposing (..)

import Json.Decode

type alias MessagePayloadJson = { message: MessagePayloadInnerJson }
type alias MessagePayloadInnerJson =
    { title     : String
    , message   : String
    , link      : String
    }

type MessagePushJet
    = MessagePayload MessagePayloadJson

messagePayloadDecoder =
    Json.Decode.map  MessagePayloadJson (Json.Decode.field "message" messagePayloadInnerDecoder)
messagePayloadInnerDecoder =
    Json.Decode.map3 MessagePayloadInnerJson (Json.Decode.field "title"   Json.Decode.string)
                                             (Json.Decode.field "message" Json.Decode.string)
                                             (Json.Decode.field "link"    Json.Decode.string)

messagePushJetDecoder json =
    case Json.Decode.decodeString messagePayloadDecoder json of
        Ok ok -> Ok (MessagePayload ok)
        Err _ -> Err "Could not parse JSON"
-- vim:ft=haskell:
