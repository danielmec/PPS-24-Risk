offensivebotattack(Territories, Neighbors, 'MainPhase', PlayerId, Action, Score, Description) :-
    member(territory(From, PlayerId, Troops), Territories),
    Troops > 5,   
    member(neighbor(From, To, EnemyId), Neighbors),
    EnemyId \= PlayerId,
    EnemyId \= none,    
    member(territory(To, EnemyId, _), Territories),  
    TroopsToUse = 3,
    Action = attack(From, To, TroopsToUse),
    Score = 1.5,
    Description = 'Attack enemy neighbor with strong territory'.
