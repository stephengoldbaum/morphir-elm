{-
   Copyright 2020 Morgan Stanley

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
-}


module Morphir.IR.Name exposing
    ( Name, fromList, toList
    , fromString, toTitleCase, toCamelCase, toSnakeCase, toHumanWords, toHumanWordsTitle
    )

{-| `Name` is an abstraction of human-readable identifiers made up of words. This abstraction
allows us to use the same identifiers across various naming conventions used by the different
frontend and backend languages Morphir integrates with.

    name = fromString "valueInUSD"

    toTitleCase name --> "ValueInUSD"
    toCamelCase name --> "valueInUSD"
    toSnakeCase name --> "value_in_usd"


## Abbreviations

We frequently use abbreviations in a business context to be more concise.
From a naming perspective abbreviations are challanging because they are not real words and
behave slightly differently. In this module we treat abbreviations as a list of single-letter
words. This approach fits nicely into camel and title case naming conventions but when using
snake-case the direct translation would look unnatural:

    toSnakeCase name -- "value_in_u_s_d" ?

To resolve this and help creating human-readable strings we added functionality to turn
abbreviations into upper-case words. We treat any series of single letter words as an
abbreviation:

    toSnakeCase name --> "value_in_USD"

@docs Name, fromList, toList


# String conversion

@docs fromString, toTitleCase, toCamelCase, toSnakeCase, toHumanWords, toHumanWordsTitle

-}

import Regex exposing (Regex)


{-| Type that represents a name that is made up of words.
-}
type alias Name =
    String


{-| Translate a string into a name by returning the input string directly.

    fromString "fooBar_baz 123"
    --> "fooBar_baz 123"

    fromString "valueInUSD"
    --> "valueInUSD"

    fromString "ValueInUSD"
    --> "ValueInUSD"

    fromString "value_in_USD"
    --> "value_in_USD"

    fromString "_-%"
    --> "_-%"

-}
fromString : String -> Name
fromString string =
    string


{-| Turns a name into a title-case string.

    toTitleCase "fooBar_baz 123"
    --> "Foobarbaz123"

    toTitleCase "valueInUSD"
    --> "Valueinusd"

-}
toTitleCase : Name -> String
toTitleCase name =
    name
        |> String.toLower
        |> capitalize


{-| Turns a name into a camel-case string.

    toCamelCase "fooBar_baz 123"
    --> "foobarbaz123"

    toCamelCase "valueInUSD"
    --> "valueinusd"

-}
toCamelCase : Name -> String
toCamelCase name =
    name
        |> String.toLower


{-| Turns a name into a snake-case string.

    toSnakeCase "fooBar_baz 123"
    --> "foo_bar_baz_123"

    toSnakeCase "valueInUSD"
    --> "value_in_usd"

-}
toSnakeCase : Name -> String
toSnakeCase name =
    name
        |> String.toLower
        |> String.split " "
        |> String.join "_"


{-| Turns a name into a list of human-readable strings. The only difference
compared to [`toList`](#toList) is how it handles abbreviations. They will
be turned into a single upper-case word instead of separate single-character
words.

    toHumanWords "valueInUSD"
    --> [ "valueinusd" ]

-}
toHumanWords : Name -> List String
toHumanWords name =
    [ name |> String.toLower ]


{-| Turns a name into a list of human-readable strings with the first word capitalized. The only difference
compared to [`toList`](#toList) is how it handles abbreviations. They will
be turned into a single upper-case word instead of separate single-character
words.

    toHumanWordsTitle "valueInUSD"
    --> [ "Valueinusd" ]

-}
toHumanWordsTitle : Name -> List String
toHumanWordsTitle name =
    [ name |> String.toLower |> capitalize ]


capitalize : String -> String
capitalize string =
    case String.uncons string of
        Just ( headChar, tailString ) ->
            String.cons (Char.toUpper headChar) tailString

        Nothing ->
            string


{-| Convert a list of strings into a name.
-}
fromList : List String -> Name
fromList words =
    String.join "" words


{-| Convert a name to a list of strings.
-}
toList : Name -> List String
toList name =
    [ name ]
