module Shared.Msg exposing (..)

import Api
import Jwt exposing (JwtError)


type Msg
    = CheckTokenExired (Result JwtError Bool)
    | Login Api.LoginResponse
    | GotClubs (Result Api.Error Api.ClubListResponse)
    | Logout