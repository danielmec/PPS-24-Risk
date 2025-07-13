offensivebotattack(Territories, Neighbors, 'MainPhase', PlayerId, Action, Score, Description) :-
    member(territory(From, PlayerId, Troops), Territories),
    Troops > 5,   
    member(neighbor(From, To, EnemyId), Neighbors),
    EnemyId \= PlayerId,
    EnemyId \= none,    
    member(territory(To, EnemyId, DefTroops), Territories),  
    TroopsToUse = 3,
    Action = attack(From, To, TroopsToUse),
    Score is Troops / (DefTroops + 0.1),
    Description = 'Attack enemy neighbor with strong territory'.