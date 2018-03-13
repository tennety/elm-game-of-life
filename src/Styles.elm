module Styles exposing (..)

import Color exposing (..)
import Style exposing (style)
import Style.Color as SColor
import Style.Font as Font


type GolStyles
    = Title
    | Body
    | Toolbar
    | Button
    | Map


stylesheet =
    Style.styleSheet
        [ style Title
            [ SColor.text darkGrey
            , Font.size 30
            , Font.typeface
                [ Font.font "Helvetica"
                ]
            ]
        , style Button
            [ SColor.text white
            , SColor.background blue
            , Font.size 16
            , Font.typeface
                [ Font.font "Helvetica"
                ]
            ]
        ]
