module Queue exposing (dequeue, empty, enqueue, fold, peak, pop, toList)

import Tuple exposing (second)


type Queue a
    = Queue (List a) (List a)


empty : Queue a
empty =
    Queue [] []


enqueue : a -> Queue a -> Queue a
enqueue item (Queue inQueue _) =
    Queue (item :: inQueue) []


peak : Queue a -> Maybe a
peak (Queue inQueue outQueue) =
    case ( inQueue, outQueue ) of
        ( inQ, [] ) ->
            case List.reverse inQ of
                [] ->
                    Nothing

                x :: _ ->
                    Just x

        ( _, x :: _ ) ->
            Just x


pop : Queue a -> Maybe (Queue a)
pop (Queue inQueue outQueue) =
    case ( inQueue, outQueue ) of
        ( inQ, [] ) ->
            case List.reverse inQ of
                [] ->
                    Nothing

                _ :: xs ->
                    Just (Queue [] xs)

        ( inQ, _ :: xs ) ->
            Just (Queue inQ xs)


dequeue : Queue a -> Maybe ( a, Queue a )
dequeue (Queue inQueue outQueue) =
    case ( inQueue, outQueue ) of
        ( inQ, [] ) ->
            case List.reverse inQ of
                [] ->
                    Nothing

                x :: xs ->
                    Just ( x, Queue [] xs )

        ( inQ, x :: xs ) ->
            Just ( x, Queue inQ xs )


fold : (a -> b -> b) -> b -> Queue a -> b
fold folder accumulator queue =
    case dequeue queue of
        Nothing ->
            accumulator

        Just ( item, newQ ) ->
            fold folder (folder item accumulator) newQ


toList : Queue a -> List a
toList =
    fold (::) []
