offensivebotplacetroops(Territories, Neighbors, 'MainPhase', PlayerId, Action, Score, Description) :-
    member(territory(Territory, PlayerId, Troops), Territories),  
    \+ (member(territory(_, PlayerId, MoreTroops), Territories), MoreTroops > Troops),  
    Action = place_troops(Territory, 1),
    Score is Troops / 10.0,
    Description = 'Place bonus troops in strongest territory for attack'.