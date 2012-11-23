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
write('Enter the number of players: '),
read(Numplayers),
write('Enter the number of cards you recieved: '),
read(Numcards),
assert(numPlayerCards(Numcards)),
entercards(Numcards).

entercards(0).
entercards(N) :-
N>0, % make sure positive number
write('Enter your first/next card: '),
read(Card),
isValidCard(Card),
assert(shownCard(1,Card)),
M is N-1,
entercards(M).


% clean database of cards
clean :- abolish(shownCard/2).

