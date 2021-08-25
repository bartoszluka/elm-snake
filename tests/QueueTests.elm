module QueueTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
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
        , test "add one and pop" <|
            \item ->
                item
                    |> flip Queue.enqueue Queue.empty
                    |> Queue.peak
                    |> Expect.equal (Just item)
        ]
