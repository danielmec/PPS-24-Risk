% OFFENSIVE BOT STRATEGY

% Attack the weakest nearby territory without counting target troops

attack(PlayerId, Territories, From, To, TroopsToUse) :-             % output: troops involved to attack
    findall(DefTroops-From-To, (                                    % finds all triads (DefTroops-From-To) => Attacks
        member(territory(From, PlayerId, BotTroops), Territories),      % takes its own territory data
        BotTroops > 1,                                                  % at least 2 troops
        neighbor(From, To),                                             % territories involved should be neighbors
        member(territory(To, DefenderId, DefTroops), Territories),      % takes target data
        DefenderId \= PlayerId                                          % target should not be the Bot
    ), Attacks),
    Attacks \= [],                                                  % attacks list should not be empty
    keysort(Attacks, [MinDefTroops-From-To|_]),                     % sorts attacks list (it checks the weakest)
    member(territory(From, PlayerId, BotTroops), Territories),      % takes its own territory data
    TroopsToUse is min(BotTroops - 1, 3).                           % involves max 3 troops