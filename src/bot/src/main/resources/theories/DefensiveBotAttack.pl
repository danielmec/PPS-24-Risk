defensivebotattack(Territories, Neighbors, 'MainPhase', PlayerId, Action, Score, Description) :-
    member(territory(From, PlayerId, BotTroops), Territories),
    BotTroops > 1,
    member(neighbor(From, To, EnemyId), Neighbors),
    EnemyId \= PlayerId,
    EnemyId \= none,
    member(territory(To, EnemyId, DefTroops), Territories),
    BotTroops >= 2 * DefTroops,  
    TroopsToUse is min(BotTroops - 1, 3),
    Action = attack(From, To, TroopsToUse),
    Score is BotTroops / (DefTroops + 0.1),
    Description = 'Attack enemy neighbor if at clear advantage'.
