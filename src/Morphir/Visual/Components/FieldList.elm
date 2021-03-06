module Morphir.Visual.Components.FieldList exposing (..)

import Element exposing (Element, centerY, el, fill, none, paddingXY, rgb, rgb255, spacing, table, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Morphir.IR.Name exposing (Name)
import Morphir.Visual.Common exposing (nameToText)


view : List ( Name, Element msg ) -> Element msg
view fields =
    table
        [ width fill
        , spacing 5
        ]
        { columns =
            [ { header = none
              , width = fill
              , view =
                    \( fieldName, _ ) ->
                        el
                            [ width fill
                            , paddingXY 10 5
                            , centerY
                            , Font.color (rgb 1 1 1)
                            , Font.bold
                            , Background.color (rgb 0.2 0.3 0.4)
                            , Border.roundEach
                                { topLeft = 6
                                , bottomLeft = 6
                                , topRight = 0
                                , bottomRight = 0
                                }
                            ]
                            (text (nameToText fieldName))
              }
            , { header = none
              , width = fill
              , view =
                    \( _, fieldValue ) ->
                        fieldValue
              }
            ]
        , data =
            fields
        }
