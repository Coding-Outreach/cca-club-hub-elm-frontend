module Shared.Model exposing (Model, LoginStatus(..))

type alias Model =
    { loginStatus : LoginStatus
    }


type LoginStatus
    = NotLoggedIn
    | LoggedIn { token : String, clubId : String }

