offensivebotreinforce(Territories, Neighbors, 'MainPhase', PlayerId, Action, Score, Description) :-
    (reinforce(PlayerId, Territories, Neighbors, From, To) ->
        Action = reinforce(From, To, 1),
        Score = 1.0,
        Description = 'Reinforce territory with most enemy neighbors'
    ;
        Action = end_turn,
        Score = 1.0,
        Description = 'No valid reinforcement found, ending turn'
    ).

reinforce(PlayerId, Territories, Neighbors, From, To) :-
    findall(EnemyCount-TerritoryName, (
        member(territory(TerritoryName, PlayerId, Troops), Territories),
        Troops > 1,
        findall(1, (
            member(neighbor(TerritoryName, N), Neighbors),
            member(territory(N, OpponentId, _), Territories),
            OpponentId \= PlayerId
        ), EnemyNeighbors),
        length(EnemyNeighbors, EnemyCount)
    ), EnemyLevels),
    EnemyLevels \= [],
    keysort(EnemyLevels, [MinEnemies-From|_]),
    findall(NegEnemyCount-TerritoryName, (
        member(EnemyCount-TerritoryName, EnemyLevels),
        NegEnemyCount is -EnemyCount
    ), NegEnemyLevels),
    keysort(NegEnemyLevels, [_-To|_]),
    From \= To.
