botsetupplacetroops(Territories, Neighbors, 'SetupPhase', PlayerId, Action, Score, Description) :-
    member(territory(Territory, PlayerId, Troops), Territories),
    \+ (member(territory(_, PlayerId, LessTroops), Territories), LessTroops < Troops),
    Action = place_troops(Territory, 1),
    Score is 2.0 / (Troops + 1),
    Description = 'Place troops on owned territory during setup'.