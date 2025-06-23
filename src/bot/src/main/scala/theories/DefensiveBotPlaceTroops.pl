% DEFENSIVE BOT STRATEGY

% Place troops on territory more in danger

place_troops(PlayerId, Territories, TerritoryInDanger) :-                           % output: first territory of sorted threat levels list
    findall(NegThreat-TerritoryName, (                                              % finds all pair NegThreat-TerritoryName
        member(territory(TerritoryName, PlayerId, _), Territories),                 % takes its own territory data, once at a time
        findall(OpponentTroops, (                                                   % collects OpponentTroops
            neighbor(TerritoryName, Neighbor),                                      % conditions to satisfy: territories involved should be neighbors
            member(territory(Neighbor, OpponentId, OpponentTroops), Territories),   % takes the opponent territory data
            OpponentId \= PlayerId                                                  % opponent should not be the Bot
        ), Threats),
        sum_list(Threats, Threat),                                                  % sums opponent troops
        NegThreat is -Threat                                                        % makes NegThreat as a negative Threat
    ), NegThreatLevels),
    NegThreatLevels \= [],                                                          % theats levels list should not be empty
    keysort(NegThreatLevels, [_-TerritoryInDanger|_]).                              % sorts territories by threat level                 