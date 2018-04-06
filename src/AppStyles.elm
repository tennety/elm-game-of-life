module AppStyles exposing (..)

import Color exposing (..)
import Style exposing (style)
import Style.Background as Background
import Style.Border as Border
import Style.Color as SColor
import Style.Font as Font


type AppStyles
    = Header
    | Title
    | Colophon
    | Body
    | Section
    | Toolbar
    | Button
    | Map


stylesheet assetRoot =
    Style.styleSheet
        [ style Body
            [ Background.coverImage (assetRoot ++ "images/retro-bg.jpg")
            , Font.typeface
                [ Font.font "Courier New"
                , Font.font "monospace"
                ]
            ]
        , style Header
            [ SColor.background darkCharcoal
            , SColor.text lightGrey
            ]
        , style Title
            [ Font.size 40
            , Font.weight 800
            ]
        , style Button
            [ SColor.text white
            , SColor.background lightBlue
            , Border.all 2
            , Border.solid
            , Border.rounded 5
            ]
        ]
