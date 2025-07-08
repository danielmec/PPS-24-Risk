offensivebotplacetroops(Territories, Neighbors, 'MainPhase', PlayerId, Action, Score, Description) :-
    member(territory(Territory, PlayerId, Troops), Territories),  
    \+ (member(territory(_, PlayerId, MoreTroops), Territories), MoreTroops > Troops),  
    Score is Troops / 10.0,
    Action = place_troops(Territory, 1),
    Description = 'Place bonus troops in strongest territory for attack'.