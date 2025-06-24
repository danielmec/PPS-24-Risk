% OFFENSIVE BOT STRATEGY

% Place troops on the territory with the most enemy neighbors => aggressive expansion

place_troops(PlayerId, Territories, TerritoryToReinforce) :-                           % output: first territory of sorted threat levels list
    findall(NegEnemyNeighbors-TerritoryName, (                                         % finds all pairs (NegEnemyNeighbors-TerritoryName) => EnemyNeighbors
        member(territory(TerritoryName, PlayerId, _), Territories),                         % takes its own territory data, once at a time
        findall(1, (                                                                        % collects EnemyNeighbors
            neighbor(TerritoryName, Neighbor),                                                  % conditions to satisfy:    territories involved should be neighbors
            member(territory(Neighbor, OpponentId, _), Territories),                                                      % takes the opponent territory data
            OpponentId \= PlayerId                                                                                        % opponent should not be the Bot
        ), EnemyNeighbors),
        length(EnemyNeighbors, EnemyCount),                                            % counts the number of enemy neighbors
        NegEnemyNeighbors is -EnemyCount                                               % makes NegEnemyNeighbors as a negative EnemyCount
    ), NegEnemyNeighborLevels),
    NegEnemyNeighborLevels \= [],                                                      % theats levels list should not be empty
    keysort(NegEnemyNeighborLevels, [_-TerritoryToReinforce|_]).                       % sorts territories by number of enemy neighbors