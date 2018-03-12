module SelectList exposing (..)


type alias SelectList a =
    { preceding : List a
    , current : a
    , following : List a
    }


singleton : a -> SelectList a
singleton elem =
    { preceding = [], current = elem, following = [] }


fromList : List a -> Maybe (SelectList a)
fromList list =
    case list of
        [] ->
            Nothing

        elem :: rest ->
            Just
                { preceding = []
                , current = elem
                , following = rest
                }


preceding : SelectList a -> List a
preceding slist =
    List.reverse slist.preceding


current : SelectList a -> a
current slist =
    slist.current


following : SelectList a -> List a
following slist =
    slist.following


toList : SelectList a -> List a
toList slist =
    (List.reverse slist.preceding) ++ [ slist.current ] ++ slist.following


next : SelectList a -> SelectList a
next slist =
    case slist.following of
        [] ->
            slist

        nextOne :: remaining ->
            { preceding = slist.current :: slist.preceding
            , current = nextOne
            , following = remaining
            }


previous : SelectList a -> SelectList a
previous slist =
    case slist.preceding of
        [] ->
            slist

        nextOne :: remaining ->
            { preceding = remaining
            , current = nextOne
            , following = slist.current :: slist.following
            }


jump : Int -> SelectList a -> SelectList a
jump amount slist =
    if amount > 0 then
        jump (amount - 1) (next slist)
    else if amount < 0 then
        jump (amount + 1) (previous slist)
    else
        slist


append : a -> SelectList a -> SelectList a
append elem slist =
    { slist | following = elem :: slist.following }


prepend : a -> SelectList a -> SelectList a
prepend elem slist =
    { slist | preceding = elem :: slist.preceding }


size : SelectList a -> Int
size slist =
    List.length slist.preceding
        + List.length slist.following
        + 1


map : (a -> b) -> SelectList a -> SelectList b
map f slist =
    { slist
        | preceding = List.map f slist.preceding
        , current = f slist.current
        , following = List.map f slist.following
    }


updateCurrent : (a -> a) -> SelectList a -> SelectList a
updateCurrent f slist =
    { slist | current = f slist.current }


removeCurrent : SelectList a -> SelectList a
removeCurrent slist =
    case ( slist.preceding, slist.following ) of
        ( _, head :: rest ) ->
            { slist | current = head, following = rest }

        ( head :: rest, _ ) ->
            { slist | current = head, preceding = rest }

        ( _, _ ) ->
            slist
