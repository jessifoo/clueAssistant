/*
Dynamic predicates to represent suspected weapons, persons, and rooms
*/

:- dynamic suspect/1.
suspect(profplum).
suspect(msscarlet).
suspect(mrspeacock).
suspect(revgreen).
suspect(mrswhite).
suspect(colmustard).

:- dynamic mweapon/1.
mweapon(knife).
mweapon(candlestick).
mweapon(revolver).
mweapon(rope).
mweapon(leadpipe).
mweapon(wrench).

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
:- dynamic shownCard/1.

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

isWeapon(knife).
isWeapon(candlestick).
isWeapon(revolver).
isWeapon(rope).
isWeapon(leadpipe).
isWeapon(wrench).

isRoom(kitchen).
isRoom(ballroom).
isRoom(conservatory).
isRoom(billiardroom).
isRoom(library).
isRoom(study).
isRoom(hall).
isRoom(lounge).
isRoom(diningroom).

isPerson(profplum).
isPerson(msscarlet).
isPerson(mrspeacock).
isPerson(revgreen).
isPerson(mrswhite).
isPerson(colmustard).

cardShown(Player,Card).

start :-
write('Enter the number of players: '),
read(Numplayers),
write('Enter the number of cards you recieved: '),
read(Numcards),
assert(numPlayerCards(Numcards)),
entercards(Numcards).

isValidCard(Card) :- isWeapon(Card) ; isRoom(Card).

entercards(0).
entercards(N) :-
N>0, % make sure positive number
write('Enter your first/next card: '),
read(Card),
isValidCard(Card),
assert(shownCard(Card)),
M is N-1,
entercards(M).

/* EXAMPLE OF HOW TO LOOP IN PROLOG.
testloop(0).
testloop(N) :- N>0, write(‘Number : ‘), write(N), nl, M is N-1, testloop(M).
*/

