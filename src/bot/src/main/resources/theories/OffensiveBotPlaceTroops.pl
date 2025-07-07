offensivebotplacetroops(Territories, Neighbors, 'MainPhase', PlayerId, Action, Score, Description) :-
    place_troops(PlayerId, Territories, Neighbors, TerritoryToReinforce),
    Action = place_troops(TerritoryToReinforce, 1),
    Score = 1.0,
    Description = 'Place troops on territory with most enemy neighbors'.

place_troops(PlayerId, Territories, Neighbors, TerritoryToReinforce) :-
    findall(NegEnemyNeighbors-TerritoryName, (
        member(territory(TerritoryName, PlayerId, _), Territories),
        findall(1, (
            member(neighbor(TerritoryName, Neighbor), Neighbors),
            member(territory(Neighbor, OpponentId, _), Territories),
            OpponentId \= PlayerId
        ), EnemyNeighbors),
        length(EnemyNeighbors, EnemyCount),
        NegEnemyNeighbors is -EnemyCount
    ), NegEnemyNeighborLevels),
    NegEnemyNeighborLevels \= [],
    keysort(NegEnemyNeighborLevels, [_-TerritoryToReinforce|_]).
