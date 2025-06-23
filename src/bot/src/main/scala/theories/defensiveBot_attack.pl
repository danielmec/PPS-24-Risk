% DEFENSIVE BOT STRATEGY

% Attack nearby territories with up to half of Bot troops

attack(PlayerId, Territories, From, To, TroopsToUse) :-
    member(territory(From, PlayerId, BotTroops), Territories),      % takes its own territory data
    BotTroops > 1,                                                  % at least 2 troops
    neighbor(From, To),                                             % territories involved should be neighbors
    member(territory(To, DefenderId, DefTroops), Territories),      % takes target data
    DefenderId \= PlayerId,                                         % target should not be the Bot
    BotTroops >= 2 * DefTroops,                                     % target troops should be up to half of Bot troops
    TroopsToUse is min(BotTroops - 1, 3).                           % involves max 3 troops