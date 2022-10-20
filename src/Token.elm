module Token exposing (TokenPayload, decodeToken, getClubIdFromToken)

import Json.Decode as D
import Jwt


type alias TokenPayload =
    { clubId : String
    , exp : Int
    }


tokenDecoder : D.Decoder TokenPayload
tokenDecoder =
    D.map2 TokenPayload (D.field "clubId" D.string) (D.field "exp" D.int)


decodeToken : String -> Result Jwt.JwtError TokenPayload
decodeToken =
    Jwt.decodeToken tokenDecoder


getClubIdFromToken : String -> Result Jwt.JwtError String
getClubIdFromToken token =
    Result.map .clubId (decodeToken token)
