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

% Current player room
:- dynamic playerRoom/1.

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
write('Which room are you starting in? '),
read(Room),
assert(playerRoom(Room)),
playLoop.

% procedure to enter cards into play
entercards(0).
entercards(N) :-
N>0, % make sure positive number
write('Enter your first/next card: '),
read(Card),
isValidCard(Card),
assert(shownCard(1,Card)),
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
write('[1] for the current recommended move (guess)'),nl,
write('[2] to enter card discovered from your turn'),nl,
write('[3] to change the room you are currently in'),nl,
write('[4] to enter an opponents failed guess'),nl,
write('[5] to show remaining possible cards'),nl,
write('[6] quit program'),nl,
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
showOptions.

executeOption(3) :-
nl,
write('You are currently in the '),
playerRoom(X),
write(X),
write('. What room would you like to move to? '),
read(Room),
retract(playerRoom(_)),!,
assert(playerRoom(Room)),
showOptions.

% PUT CODE TO PROCESS AN OPPONENTS GUESS HERE ////
executeOption(4).

% show remaining cards again
executeOption(5) :- printAvailCards.

executeOption(6) :- retractall(shownCard(_,_)), retractall(numPlayerCards(_)), retractall(numPlayers(_)), retractall(playerRoom(_)), false.

% Lists all the cards that have not been shown yet
printAvailCards :- printSuspects ; printRooms ; printWeapons.
printAvailCards :- showOptions.

printSuspects :-
nl,
write('Current Suspects: '),nl,nl,
suspect(X),
not(shownCard(1,X)),
write(X),
nl,
fail.

printWeapons :-
nl,
write('Possible Weapons: '),nl,nl,
mweapon(X),
not(shownCard(1,X)),
write(X),
nl,
fail.

printRooms :-
nl,
write('Possible Rooms: '),nl,nl,
mroom(X),
not(shownCard(1,X)),
write(X),
nl,
fail.

% clean database of cards
clean :- abolish(shownCard/2).

/* NOTES and ideas

each card (person,weapon,room) will be represented by a two arrity predicate of the form (id,eval). Where eval represents the probability of it being correct (higher is LESS LIKELY)

1. If it is the players turn they can execute a card ENTRY and cards can be removed
from play. Also, if a card is PROPOSED and not shown, it can be entered directly as a solution.

2. If it is NOT the players turn we are not shown the ID of any cards (we can not enter it into the system). Instead the player enters a query of the form (suspect,person,weapon,number) where number represents how many UNKNOWN cards are held up.

    THEN, for each ID in the query we change the predicate to (id,eval+NUMBER) everytime a query is made and an unknown card is held up it increases the chance that the ID is NOT valid.

EXAMPLE quick game.

(profplum, 0)
(mrspeacock, 0)
(mrswhite, 0)
(wrench, 0)
(revolver, 0)
(rope, 0)
(hall, 0)
(diningroom, 0)
(study, 0)

MY Turn
(profplum, rope, study) - I am shown prof plum and rope
* remove profplum
* remove rope
* confirm study (i.e. remove all other)

(mrspeacock, 0)
(mrswhite, 0)
(wrench, 0)
(revolver, 0)
(study, 0)

OTHER Player's Turn
(mrspeacock, wrench, hall, 1) - ONE unknown is held up
* add 1 to mrspeacock
* add 1 to wrench
* ignore hall since it's not in database anymore

(mrspeacock, 1)
(mrswhite, 0)
(wrench, 1)
(revolver, 0)
(study, 0)

RECOMMENDED PLAY

(mrswhite, revolver, study) - THESE ARE NOW THE MOST LIKELY.


*/

