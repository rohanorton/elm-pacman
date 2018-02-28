module Mode exposing (Mode)

{-| Ghosts alternate between scatter and chase modes during gameplay at
    predetermined intervals. These mode changes are easy to spot as the ghosts
    reverse direction when they occur. Scatter modes happen four times per
    level before the ghosts stay in chase mode indefinitely.
-}


type Mode
    = Scatter ActionTime
    | Chase ActionTime
    | Frightened


type ActionTime
    = For Int
    | Indefinitely


type alias GamePlan =
    { level : Int
    , plan : List Mode
    }


gamePlans : List GamePlan
gamePlans =
    [ { level = 1, plan = [ Scatter (For 7), Chase (For 20), Scatter (For 7), Chase (For 20), Scatter (For 5), Chase Indefinitely ] }
    , { level = 2, plan = [ Scatter (For 7), Chase (For 20), Scatter (For 7), Chase (For 1033), Chase Indefinitely ] }
    , { level = 5, plan = [ Scatter (For 5), Chase (For 20), Scatter (For 5), Chase (For 1037), Chase Indefinitely ] }
    ]
