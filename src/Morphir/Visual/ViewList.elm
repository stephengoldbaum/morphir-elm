module Morphir.Visual.ViewList exposing (view)

import Dict
import Element exposing (Element, fill, none, spacing, table, text)
import Morphir.IR.Name as Name
import Morphir.IR.Type as Type exposing (Type)
import Morphir.IR.Value as Value exposing (Value)


view : (Value ta (Type ta) -> Element msg) -> Type ta -> List (Value ta (Type ta)) -> Element msg
view viewValue itemType items =
    case itemType of
        Type.Record _ fields ->
            table
                [ spacing 10
                ]
                { data = items
                , columns =
                    fields
                        |> List.map
                            (\field ->
                                { header = text (field.name |> Name.toHumanWords |> String.join " ")
                                , width = fill
                                , view =
                                    \item ->
                                        -- TODO: Use interpreter to get field values
                                        case item of
                                            Value.Record _ fieldValues ->
                                                fieldValues
                                                    |> Dict.fromList
                                                    |> Dict.get field.name
                                                    |> Maybe.map viewValue
                                                    |> Maybe.withDefault (text "???")

                                            _ ->
                                                viewValue item
                                }
                            )
                }

        _ ->
            table
                [ spacing 10
                ]
                { data = items
                , columns =
                    [ { header = none
                      , width = fill
                      , view = viewValue
                      }
                    ]
                }