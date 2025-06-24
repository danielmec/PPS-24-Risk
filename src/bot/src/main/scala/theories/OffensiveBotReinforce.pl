% OFFENSIVE BOT STRATEGY

% Reinforce moving troops from the safest territory to the one with the most enemy neighbors => aggressive expansion

reinforce(PlayerId, Territories, From, To) :-                                 % output: territories involved in troops move
    findall(EnemyCount-TerritoryName, (                                       % finds all pairs (EnemyCount-TerritoryName) => EnemyLevels
        member(territory(TerritoryName, PlayerId, Troops), Territories),           % takes its own territory data, once at a time
        Troops > 1,                                                                % at least 2 troops
        findall(1, (                                                               % collects EnemyNeighbors
            neighbor(TerritoryName, N),                                                 % conditions to satisfy:    territories involved should be neighbors
            member(territory(N, OpponentId, _), Territories),                                                     % takes the opponent territory data
            OpponentId \= PlayerId                                                                                % opponent should not be the Bot
        ), EnemyNeighbors),
        length(EnemyNeighbors, EnemyCount)                                        % counts the number of enemy neighbors
    ), EnemyLevels),
    EnemyLevels \= [],                                                            % enemy levels list should not be empty
    keysort(EnemyLevels, [MinEnemies-From|_]),                                    % sorts enemy levels list (safest: fewest enemy neighbors)
    findall(NegEnemyCount-TerritoryName, (                                        % finds all pairs (NegEnemyCount-TerritoryName) => NegEnemyLevels
        member(EnemyCount-TerritoryName, EnemyLevels),                                % takes pairs data
        NegEnemyCount is -EnemyCount                                                  % makes NegEnemyCount as a negative EnemyCount
    ), NegEnemyLevels),
    keysort(NegEnemyLevels, [_-To|_]),                                            % sorts by most enemy neighbors
    From \= To.                                                                   % territories involved in troops move should not be the same