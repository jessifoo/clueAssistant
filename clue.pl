/*
Dynamic predicates to represent suspected weapons, persons, and rooms
*/

% Current Suspects
:- dynamic suspect/1.
suspect(profplum).
suspect(msscarlet).
suspect(mrspeacock).
suspect(revgreen).
suspect(mrswhite).
suspect(colmustard).

% Current Possible Weapons
:- dynamic mweapon/1.
mweapon(knife).
mweapon(candlestick).
mweapon(revolver).
mweapon(rope).
mweapon(leadpipe).
mweapon(wrench).

% Current Possible Rooms
:- dynamic mroom/1.
mroom(kitchen).
mroom(ballroom).
mroom(conservatory).
mroom(billiardroom).
mroom(library).
mroom(study).
mroom(hall).
mroom(lounge).
mroom(diningroom).

% List of all cards which have been shown, shown cards are eliminated from solution.
:- dynamic shownCard/2.

% Number of cards each player has.
:- dynamic numPlayerCards/1.

% Number of players in the game
:- dynamic numPlayers/1.

% SOLUTION predicate
%murderer(X) :- suspect(),weapon(),room()

/*
Build predicate to take in game init: num players, who starts, cards given
*/

notWeapon(X) :- not(isWeapon(X)).
notRoom(X) :- not(isRoom(X)).
notValidPerson(X) :- not(isPerson(X)).

% Valid weapon check
isWeapon(knife).
isWeapon(candlestick).
isWeapon(revolver).
isWeapon(rope).
isWeapon(leadpipe).
isWeapon(wrench).

% Valid room check
isRoom(kitchen).
isRoom(ballroom).
isRoom(conservatory).
isRoom(billiardroom).
isRoom(library).
isRoom(study).
isRoom(hall).
isRoom(lounge).
isRoom(diningroom).

% Valid person check
isPerson(profplum).
isPerson(msscarlet).
isPerson(mrspeacock).
isPerson(revgreen).
isPerson(mrswhite).
isPerson(colmustard).

% Valid card check
isValidCard(Card) :- isWeapon(Card) ; isRoom(Card) ; isPerson(Card).

% BEGIN GAMEPLAY PREDICATES ---------------------

start :-
write('Welcome to the Clue Assistant program. ========='),nl,nl,
write('Enter the number of players: '),
read(Players),
assert(numPlayers(Players)),
write('Enter the number of cards you recieved: '),
read(Numcards),
assert(numPlayerCards(Numcards)),
entercards(Numcards),
playLoop.

% procedure to enter cards into play
entercards(0).
entercards(N) :-
N>0, % make sure positive number
write('Enter your first/next card: '),
read(Card),
isValidCard(Card),
assert(shownCard(1,Card)),
removeFromPlay(Card),
M is N-1,
entercards(M).

% removes a card that has been entered from possible solutions.
removeFromPlay(X) :- isWeapon(X), retract(mweapon(X)).
removeFromPlay(X) :- isPerson(X), retract(suspect(X)).
removeFromPlay(X) :- isRoom(X), retract(mroom(X)).

% loop for gameplay
playLoop :- showOptions.

% menu for asking for recommended move or enter more cards into play
showOptions :-
nl,
write('MENU ------------------------------'),nl,
write('[1] for a recommended move (guess)'),nl,
write('[2] for a card entry'),nl,
write('[3] to show remaining cards'),nl,
write('[4] quit program'),nl,
write(':'),
read(Option),
executeOption(Option).

% helper for showOptions executes the selected option.
executeOption(1) :-
nl,
write('A good guess is: '),
suspect(X),
mweapon(Y),
mroom(Z),
write(X),
write(' with the '),
write(Y),
write(' in the '),
write(Z),
!, % cut so that it only gives one possible guess per execution.
nl,
showOptions.

executeOption(2) :-
nl,
entercards(1),
nl,
write('Would you like to enter a another card [Y/N]? : '),
read(Ans),
enterAnotherCard(Ans).

% show remaining cards again
executeOption(3) :- printAvailCards.

executeOption(4) :- false.

% HELPER for executeOption(2) whether to enter another card or go back to showOptions
enterAnotherCard(X) :- X = 'Y' ; X = 'y',
executeOption(2).
enterAnotherCard(X) :- X = 'N' ; X = 'n',
showOptions.

% Lists all the cards that have not been shown yet
printAvailCards :- printSuspects ; printRooms ; printWeapons.
printAvailCards :- showOptions.

printSuspects :-
nl,
write('Current Suspects: '),nl,nl,
suspect(X),
write(X),
nl,
fail.

printWeapons :-
nl,
write('Possible Weapons: '),nl,nl,
mweapon(X),
write(X),
nl,
fail.

printRooms :-
nl,
write('Possible Rooms: '),nl,nl,
mroom(X),
write(X),
nl,
fail.

% clean database of cards
clean :- abolish(shownCard/2).

