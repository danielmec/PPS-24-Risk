offensivebotreinforce(Territories, Neighbors, 'MainPhase', PlayerId, Action, Score, Description) :-
    member(territory(From, PlayerId, Troops), Territories),
    Troops > 1,
    \+ (member(territory(_, PlayerId, MoreTroops), Territories), MoreTroops > Troops), 
    member(neighbor(From, To, PlayerId), Neighbors),
    member(territory(To, PlayerId, _), Territories), 
    (Troops > 20 -> 
        TroopsToMove = 5
    ; Troops > 10 ->
        TroopsToMove = 2
    ;
        TroopsToMove = 1
    ),   
    TroopsToMove < Troops,
    Action = reinforce(From, To, TroopsToMove),
    Score = 1.0,
    Description = 'Move troops from strongest territory to neighbor'.