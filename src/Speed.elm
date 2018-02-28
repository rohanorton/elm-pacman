module Speed exposing (Speed)


type alias Speed =
    {- Aliasing a primitive like this doesn't really give any type safety, but
       might make it a little quicker to refactor if I decide to change the
       representation of speed at a later date.
    -}
    Float
