defensivebotreinforce(Territories, Neighbors, 'MainPhase', PlayerId, Action, Score, Description) :-
    (reinforce(PlayerId, Territories, Neighbors, From, To) ->
        Action = reinforce(From, To, 1),
        Score = 1.0,
        Description = 'Reinforce moving troops from safest territory to the most endangered one'
    ;
        Action = end_turn,
        Score = 1.0,
        Description = 'No valid reinforcement found, ending turn'
    ).

reinforce(PlayerId, Territories, Neighbors, From, To) :-
    findall(Threat-TerritoryName, (
        member(territory(TerritoryName, PlayerId, Troops), Territories),
        Troops > 1,
        findall(OpponentTroops, (
            member(neighbor(TerritoryName, N), Neighbors),
            member(territory(N, OpponentId, OpponentTroops), Territories),
            OpponentId \= PlayerId
        ), Threats),
        sum_list(Threats, Threat)
    ), ThreatLevels),
    ThreatLevels \= [],
    keysort(ThreatLevels, [MinThreat-From|_]),
    findall(NegThreat-TerritoryName, (
        member(Threat-TerritoryName, ThreatLevels),
        NegThreat is -Threat
    ), NegThreatLevels),
    keysort(NegThreatLevels, [_-To|_]),
    From \= To.
