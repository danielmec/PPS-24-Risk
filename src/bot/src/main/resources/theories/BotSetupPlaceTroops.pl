botsetupplacetroops(Territories, Neighbors, 'SetupPhase', PlayerId, Action, Score, Description) :-
    % Trova tutti i territori posseduti dal bot
    member(territory(Territory, PlayerId, _), Territories),
    Action = place_troops(Territory, 1),
    Score = 1.0,
    Description = 'Place troops on owned territory during setup'.