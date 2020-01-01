module Zipper exposing
    ( Zipper
    , getCurrent
    , getNext
    , getPrevious
    , makeZipper
    , moveBackwards
    , moveBackwardsTo
    , moveBackwardsWhile
    , moveForward
    , moveForwardTo
    , moveForwardWhile
    )

import List exposing (reverse)
import List.Extra exposing (init, last)
import Maybe exposing (andThen, map, withDefault)


type alias Zipper a =
    { previous : List a
    , current : a
    , next : List a
    }


makeZipper : List a -> a -> List a -> Zipper a
makeZipper =
    -- maybe a better name is "sortZipper"
    Zipper << reverse


moveBackwards : Zipper a -> Maybe (Zipper a)
moveBackwards { previous, current, next } =
    -- maybe we should make it impossible to give a zipper which is not sorted by `makeZipper`
    case previous of
        [] ->
            Nothing

        head :: tail ->
            Just <| Zipper tail head <| current :: next


moveForward : Zipper a -> Maybe (Zipper a)
moveForward zipper =
    map flip << moveBackwards <| flip zipper


flip : Zipper a -> Zipper a
flip { previous, current, next } =
    -- figure if next needs to be reversed
    Zipper (reverse next) current <| reverse previous


moveForwardTo : a -> Zipper a -> Maybe (Zipper a)
moveForwardTo wantedCurrent zipper =
    if wantedCurrent == zipper.current then
        Just zipper

    else
        andThen (moveForwardTo wantedCurrent) <| moveForward zipper


moveBackwardsTo : a -> Zipper a -> Maybe (Zipper a)
moveBackwardsTo wantedCurrent zipper =
    if wantedCurrent == zipper.current then
        Just zipper

    else
        andThen (moveBackwardsTo wantedCurrent) <| moveBackwards zipper


moveForwardWhile : (a -> Bool) -> Zipper a -> Maybe (Zipper a)
moveForwardWhile ifCondition zipper =
    if ifCondition zipper.current then
        andThen (moveForwardWhile ifCondition) <| moveForward zipper

    else
        Just zipper


moveBackwardsWhile : (a -> Bool) -> Zipper a -> Maybe (Zipper a)
moveBackwardsWhile ifCondition zipper =
    if ifCondition zipper.current then
        andThen (moveBackwardsWhile ifCondition) <| moveBackwards zipper

    else
        Just zipper


getPrevious : Zipper a -> List a
getPrevious =
    .previous


getCurrent : Zipper a -> a
getCurrent =
    .current


getNext : Zipper a -> List a
getNext =
    .next
