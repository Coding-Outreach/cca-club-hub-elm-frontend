module Components.Input exposing (button, currentPassword, inputBoxStyles, label, multiline, text, username)

import Color exposing (..)
import Element as E
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input



-- This is a wrapper for elm-ui.Input since we want some default styles


button : List (E.Attribute msg) -> { onPress : Maybe msg, label : E.Element msg } -> E.Element msg
button attrs options =
    Input.button
        ([ Bg.color red_500
         , E.paddingXY 16 12
         , Font.size 18
         , Font.bold
         , Border.rounded 4
         , E.focused [ Bg.color red_600 ]
         , E.mouseOver [ Bg.color red_600 ]
         ]
            ++ attrs
        )
        options


text :
    List (E.Attribute msg)
    ->
        { onChange : String -> msg
        , text : String
        , placeholder : Maybe (Input.Placeholder msg)
        , label : String
        }
    -> E.Element msg
text attr options =
    Input.text (attr ++ inputBoxStyles)
        { onChange = options.onChange
        , text = options.text
        , placeholder = options.placeholder
        , label = label options.label
        }


multiline :
    List (E.Attribute msg)
    ->
        { onChange : String -> msg
        , text : String
        , placeholder : Maybe (Input.Placeholder msg)
        , label : String
        , spellcheck : Bool
        }
    -> E.Element msg
multiline attrs options =
    Input.multiline (attrs ++ inputBoxStyles)
        { onChange = options.onChange
        , text = options.text
        , placeholder = options.placeholder
        , label = label options.label
        , spellcheck = options.spellcheck
        }


username :
    List (E.Attribute msg)
    ->
        { onChange : String -> msg
        , text : String
        , placeholder : Maybe (Input.Placeholder msg)
        , label : String
        }
    -> E.Element msg
username attr options =
    Input.username (attr ++ inputBoxStyles)
        { onChange = options.onChange
        , text = options.text
        , placeholder = options.placeholder
        , label = label options.label
        }


currentPassword :
    List (E.Attribute msg)
    ->
        { onChange : String -> msg
        , text : String
        , placeholder : Maybe (Input.Placeholder msg)
        , label : String
        , show : Bool
        }
    -> E.Element msg
currentPassword attr options =
    Input.currentPassword (attr ++ inputBoxStyles)
        { onChange = options.onChange
        , text = options.text
        , placeholder = options.placeholder
        , label = label options.label
        , show = options.show
        }


label : String -> Input.Label msg
label name =
    Input.labelAbove [ Font.bold, Font.color mono_400, Font.size 14 ] (E.text name)


inputBoxStyles : List (E.Attribute msg)
inputBoxStyles =
    [ Bg.color mono_600, Border.width 2, Border.color mono_500 ]
