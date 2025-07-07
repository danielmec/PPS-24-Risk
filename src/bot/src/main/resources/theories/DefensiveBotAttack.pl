defensivebotattack(Territories, Neighbors, 'MainPhase', PlayerId, Action, Score, Description) :-
    (attack(PlayerId, Territories, Neighbors, From, To, TroopsToUse) ->
        Action = attack(From, To, TroopsToUse),
        Score = 1.0,
        Description = 'Attack nearby territories with up to half of Bot troops'
    ;
        Action = end_turn,
        Score = 1.0,
        Description = 'No valid attack found, ending turn'
    ).

attack(PlayerId, Territories, Neighbors, From, To, TroopsToUse) :-
    member(territory(From, PlayerId, BotTroops), Territories),
    BotTroops > 1,
    member(neighbor(From, To), Neighbors),
    member(territory(To, DefenderId, DefTroops), Territories),
    DefenderId \= PlayerId,
    BotTroops >= 2 * DefTroops,
    TroopsToUse is min(BotTroops - 1, 3).
