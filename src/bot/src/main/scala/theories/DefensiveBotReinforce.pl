% DEFENSIVE BOT STRATEGY

% Reinforce moving troops from safest territory to the most endangered one 

reinforce(PlayerId, Territories, From, To) :-                                 % output: territories involved in troops move
    findall(Threat-TerritoryName, (                                           % finds all pairs (Threat-TerritoryName) => ThreatLevels
        member(territory(TerritoryName, PlayerId, Troops), Territories),            % takes its own territory data, once at a time      
        Troops > 1,                                                                 % at least 2 troops
        findall(OpponentTroops, (                                                   % collects opponent troops
            neighbor(TerritoryName, N),                                                 % conditions to satisfy:      territories involved should be neighbors
            member(territory(N, OpponentId, OpponentTroops), Territories),                                          % takes the opponent territory data
            OpponentId \= PlayerId                                                                                  % opponent should not be the Bot
        ), Threats),
        sum_list(Threats, Threat)                                                   % sums the opponent troops
    ), ThreatLevels),
    ThreatLevels \= [],                                                       % theats levels list should not be empty
    keysort(ThreatLevels, [MinThreat-From|_]),                                % sorts threats levels list (it checks the safest)                      
    findall(NegThreat-TerritoryName, (                                        % finds all pairs (NegThreat-TerritoryName) => NegThreatLevels
        member(Threat-TerritoryName, ThreatLevels),                                 % takes pairs data                     
        NegThreat is -Threat                                                        % makes NegThreat as a negative Threat
    ), NegThreatLevels),            
    keysort(NegThreatLevels, [_-To|_]),                                       % sorts threat levels list (it checks the most endangered)
    From \= To.                                                               % territories involved in troops move should not be the same