module QueueTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Http exposing (Expect)
import Queue
import Test exposing (..)


flip : (c -> b -> a) -> b -> c -> a
flip f x y =
    f y x


suite : Test
suite =
    describe "The Queue module"
        [ test "empty queue" <|
            \_ ->
                let
                    empty =
                        Queue.empty
                in
                Expect.equal Queue.empty empty
        , fuzz int "add one and pop" <|
            \fuzzInt ->
                fuzzInt
                    |> flip Queue.enqueue Queue.empty
                    |> Queue.peak
                    |> Expect.equal (Just fuzzInt)
        , fuzz (list int) "fill queue" <|
            \fuzzList ->
                let
                    dequeued =
                        List.foldl Queue.enqueue Queue.empty
                            >> Queue.toList
                            >> List.reverse
                in
                Expect.equalLists (dequeued fuzzList) fuzzList
        ]
