defensivebotreinforce(Territories, Neighbors, 'MainPhase', PlayerId, Action, Score, Description) :-
    % Trova il territorio più forte da cui spostare truppe
    member(territory(From, PlayerId, FromTroops), Territories),
    FromTroops > 1,
    \+ (member(territory(_, PlayerId, MoreTroops), Territories), MoreTroops > FromTroops),
    member(neighbor(From, To, PlayerId), Neighbors),
    member(territory(To, PlayerId, ToTroops), Territories),
    \+ (member(neighbor(From, OtherTo, PlayerId), Neighbors),
        member(territory(OtherTo, PlayerId, OtherToTroops), Territories),
        OtherToTroops < ToTroops
    ),
    (FromTroops > 20 -> TroopsToMove = 5
    ; FromTroops > 10 -> TroopsToMove = 2
    ; TroopsToMove = 1),
    TroopsToMove < FromTroops,
    Action = reinforce(From, To, TroopsToMove),
    Score is 2.0 / (ToTroops + 1),
    Description = 'Move troops from strongest to weakest territory'.
