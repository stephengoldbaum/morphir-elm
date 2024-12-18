module Morphir.IR.NameTests exposing (..)

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


import Expect
import Json.Encode exposing (encode)
import Json.Decode exposing (decodeString)
import Morphir.IR.Name as Name
import Morphir.IR.Name.Codec exposing (encodeName, decodeName)
import Test exposing (..)


fromStringTests : Test
fromStringTests =
    let
        assert inString outString =
            test ("From string " ++ inString) <|
                \_ ->
                    Name.fromString inString
                        |> Expect.equal outString
    in
    describe "fromString"
        [ assert "fooBar_baz 123" "fooBar_baz 123"
        , assert "valueInUSD" "valueInUSD"
        , assert "ValueInUSD" "ValueInUSD"
        , assert "value_in_USD" "value_in_USD"
        , assert "_-% " "_-% "
        ]


toTitleCaseTests : Test
toTitleCaseTests =
    let
        assert inString outString =
            test ("Title case " ++ outString) <|
                \_ ->
                    Name.fromString inString
                        |> Name.toTitleCase
                        |> Expect.equal outString
    in
    describe "toTitleCase"
        [ assert "fooBar_baz 123" "Foobarbaz123"
        , assert "valueInUSD" "Valueinusd"
        ]


toCamelCaseTests : Test
toCamelCaseTests =
    let
        assert inString outString =
            test ("Camel case " ++ outString) <|
                \_ ->
                    Name.fromString inString
                        |> Name.toCamelCase
                        |> Expect.equal outString
    in
    describe "toCamelCase"
        [ assert "fooBar_baz 123" "foobarbaz123"
        , assert "valueInUSD" "valueinusd"
        ]


toSnakeCaseTests : Test
toSnakeCaseTests =
    let
        assert inString outString =
            test ("Snake case " ++ outString) <|
                \_ ->
                    Name.fromString inString
                        |> Name.toSnakeCase
                        |> Expect.equal outString
    in
    describe "toSnakeCase"
        [ assert "fooBar_baz 123" "foo_bar_baz_123"
        , assert "valueInUSD" "value_in_usd"
        ]


toHumanWordsTests : Test
toHumanWordsTests =
    let
        assert inString outList =
            test ("Human words " ++ (outList |> String.join " ")) <|
                \_ ->
                    Name.fromString inString
                        |> Name.toHumanWords
                        |> Expect.equal outList
    in
    describe "toHumanWords"
        [ assert "fooBar_baz 123" [ "foobarbaz123" ]
        , assert "valueInUSD" [ "valueinusd" ]
        ]


encodeNameTests : Test
encodeNameTests =
    let
        assert inString expectedText =
            test ("encodeName " ++ (expectedText ++ " ")) <|
                \_ ->
                    Name.fromString inString
                        |> encodeName
                        |> encode 0
                        |> Expect.equal expectedText
    in
    describe "encodeName"
        [ assert "deltaSigmaTheta" """\"deltaSigmaTheta\""""
        , assert "sigmaGammaRo" """\"sigmaGammaRo\""""
        ]


decodeNameTests : Test
decodeNameTests =
    let
        assert jsonString expectedName =
            test ("decodeName " ++ jsonString) <|
                \_ ->
                    decodeString decodeName jsonString
                        |> Result.mapError (always "Decoding failed")
                        |> Expect.equal (Ok expectedName)
    in
    describe "decodeName"
        [ assert "\"deltaSigmaTheta\"" "deltaSigmaTheta"
        , assert "\"sigmaGammaRo\"" "sigmaGammaRo"
        ]
