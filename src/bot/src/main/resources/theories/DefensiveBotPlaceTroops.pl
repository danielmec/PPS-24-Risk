defensivebotplacetroops(Territories, Neighbors, 'MainPhase', PlayerId, Action, Score, Description) :-
    place_troops(PlayerId, Territories, Neighbors, TerritoryInDanger),
    Action = place_troops(TerritoryInDanger, 1),
    Score = 1.0,
    Description = 'Place troops on most endangered territory'.

place_troops(PlayerId, Territories, Neighbors, TerritoryInDanger) :-
    findall(NegThreat-TerritoryName, (
        member(territory(TerritoryName, PlayerId, _), Territories),
        findall(OpponentTroops, (
            member(neighbor(TerritoryName, Neighbor), Neighbors),
            member(territory(Neighbor, OpponentId, OpponentTroops), Territories),
            OpponentId \= PlayerId
        ), Threats),
        sum_list(Threats, Threat),
        NegThreat is -Threat
    ), NegThreatLevels),
    NegThreatLevels \= [],
    keysort(NegThreatLevels, [_-TerritoryInDanger|_]).
