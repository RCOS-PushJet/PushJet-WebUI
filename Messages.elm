module Messages exposing (..)

import Bootstrap.Alert as Alert
import Json.Decode


type alias MessagePushJet =
    { message : MessagePayloadInnerJson
    , alert : Alert.Visibility
    }


type alias MessagePayloadInnerJson =
    { title : String
    , message : String
    , link : String
    , level : Int
    , timestamp : Int
    }


messagePayloadDecoder =
    Json.Decode.map2 MessagePushJet
        (Json.Decode.field "message" messagePayloadInnerDecoder)
        (Json.Decode.succeed Alert.shown)


messagePayloadInnerDecoder =
    Json.Decode.map5 MessagePayloadInnerJson
        (Json.Decode.field "title" Json.Decode.string)
        (Json.Decode.field "message" Json.Decode.string)
        (Json.Decode.field "link" Json.Decode.string)
        (Json.Decode.field "level" Json.Decode.int)
        (Json.Decode.field "timestamp" Json.Decode.int)


messagePushJetDecoder json =
    case Json.Decode.decodeString messagePayloadDecoder json of
        Ok ok ->
            Ok ok

        Err _ ->
            Err "Could not parse JSON"

-- vim:ft=haskell:
