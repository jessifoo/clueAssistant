/*
Dynamic predicates to represent suspected weapons, persons, and rooms
*/

% Current Suspects
:- dynamic suspect/2.
suspect(profplum,0).
suspect(msscarlet,0).
suspect(mrspeacock,0).
suspect(revgreen,0).
suspect(mrswhite,0).
suspect(colmustard,0).

% Current Possible Weapons
:- dynamic mweapon/2.
mweapon(knife,0).
mweapon(candlestick,0).
mweapon(revolver,0).
mweapon(rope,0).
mweapon(leadpipe,0).
mweapon(wrench,0).

% Current Possible Rooms
:- dynamic mroom/2.
mroom(kitchen,0).
mroom(ballroom,0).
mroom(conservatory,0).
mroom(billiardroom,0).
mroom(library,0).
mroom(study,0).
mroom(hall,0).
mroom(lounge,0).
mroom(diningroom,0).

% Current player room
:- dynamic playerRoom/1.

% List of all cards which have been shown, shown cards are eliminated from solution.
% format: shownCard(player,Card)
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
write('[3] to enter or leave a room'),nl,
write('[4] to enter an opponents guess'),nl,
write('[5] to show remaining possible cards'),nl,
write('[6] quit program'),nl,
write(':'),
read(Option),
executeOption(Option).

% MIN Value Finder
min(X) :- suspect(X,Z),not((suspect(X,Other),Other<Z)),!.
min(X) :- mweapon(X,Z),not((mweapon(X,Other),Other<Z)),!.
min(X) :- mroom(X,Z),not((mroom(X,Other),Other<Z)),!.

% helper for showOptions executes the selected option.
% Gives the best guess based on the evaluator functions of the items in database
executeOption(1) :-
nl,
write('A good guess is: '),
suspect(X,_),min(X),
mweapon(Y,_),min(Y),
mroom(Z,_),min(Z),
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

%enter or leave a room
executeOption(3) :-
nl,
write('You are currently in the '),
%begin if-then-else
(playerRoom(X) -> write(X),
write('. Leave room [Y/N]? '),
read(Ans),
((Ans = 'Y' ; Ans = 'y') -> retract(playerRoom(_)),! ; true), showOptions ;
write('corridor. Which room would you like to enter? '),
read(Room),
assert(playerRoom(Room))),
showOptions.

% PUT CODE TO PROCESS AN OPPONENTS GUESS HERE ////
executeOption(4) :-
nl,
write('Enter your opponents guess in the form [person,weapon,room] : '),
read(GuessArray),
opponentGuess(GuessArray),
showOptions.

% show remaining cards again
executeOption(5) :- printAvailCards.

executeOption(6) :- retractall(shownCard(_,_)), retractall(numPlayerCards(_)), retractall(numPlayers(_)), retractall(playerRoom(_)), false.

% HELPER for executeOption(4) to process an opponents guess
% modify suspect entry
opponentGuess([H|T]) :-
isPerson(H),
(not(shownCard(_,H)) -> retract(suspect(H,Y)),
incr(Y,Y1), assert(suspect(H,Y1)) ; true),
opponentGuess(T).
opponentGuess([H|T]) :-
isWeapon(H),
(not(shownCard(_,H)) -> retract(mweapon(H,Y)),
incr(Y,Y1),assert(mweapon(H,Y1)) ; true),
opponentGuess(T).
opponentGuess([H]) :-
isRoom(H),
(not(shownCard(_,H)) -> retract(mroom(H,Y)),
incr(Y,Y1),assert(mroom(H,Y1)) ; true).

% number incrementer
incr(X,X1) :- X1 is X+1.

% Lists all the cards that have not been shown yet
printAvailCards :- printSuspects ; printRooms ; printWeapons.
printAvailCards :- showOptions.

printSuspects :-
nl,
write('Current Suspects: '),nl,nl,
suspect(X,_),
not(shownCard(1,X)),
write(X),
nl,
fail.

printWeapons :-
nl,
write('Possible Weapons: '),nl,nl,
mweapon(X,_),
not(shownCard(1,X)),
write(X),
nl,
fail.

printRooms :-
nl,
write('Possible Rooms: '),nl,nl,
mroom(X,_),
not(shownCard(1,X)),
write(X),
nl,
fail.

% clean database of cards
clean :- abolish(shownCard/2).

/* NOTES and ideas


*/

