/*
Dynamic predicates to represent suspected weapons, persons, and rooms
*/

% Room player is currently in (no room = corridor)
:- dynamic playerRoom/1.

/* List of all cards which have been shown or held up, shown cards are assigned prob of 100%
and eliminated from solution. An unknown card which is held up is assigned a probability
that the player has that card. shownCard(player,Card,probability) */
:- dynamic shownCard/3.

% Number of cards each player has.
:- dynamic numPlayerCards/1.

% Number of players in the game
:- dynamic numPlayers/1.

% holds the cards from opponent an guess
:- dynamic guessCards/1.

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

% START program and enter play loop.
start :-
write('Welcome to the Clue Assistant program. ========='),nl,nl,
write('Enter the number of players: '),
read(Players),
assert(numPlayers(Players)),
write('Enter the number of cards you recieved: '),
read(Numcards),
assert(numPlayerCards(Numcards)),
entercards(1,Numcards),
playLoop.

% ENTERCARDS procedure to enter KNOWN cards (Player, Number of Cards to Enter)
entercards(_,0).
entercards(P,N) :-
N>0, % make sure positive number
write('Enter your first/next card: '),
read(Card),
isValidCard(Card),
assert(shownCard(P,Card,1.0)),
M is N-1,
entercards(P,M).

% PLAYLOOP for gameplay
playLoop :- showOptions.

% SHOWOPTIONS - MAIN MENU ===================
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

% MIN Value Finder for evaluated numbers attached to Cards
min(X) :- isPerson(X),shownCard(_,X,Z),not((shownCard(_,X,Other),Other<Z)),!.
min(X) :- isWeapon(X),shownCard(_,X,Z),not((shownCard(_,X,Other),Other<Z)),!.
min(X) :- isRoom(X),shownCard(_,X,Z),not((shownCard(_,X,Other),Other<Z)),!.

% HELPER for showOptions (MENU) executes selected option.

% EXECUTEOPTION[1] Gives the best guess based on the evaluator functions of the items in database
executeOption(1) :-
nl,
write('A good guess is: '),
% takes an unshown card or the lowest probability card
isPerson(X),(not(shownCard(_,X,_)) -> true ; min(X),not(shownCard(_,X,1.0))),
isWeapon(Y),(not(shownCard(_,Y,_)) -> true ; min(Y),not(shownCard(_,Y,1.0))),
isRoom(Z),(not(shownCard(_,Z,_)) -> true ; min(Z),not(shownCard(_,Z,1.0))),
write(X),
write(' with the '),
write(Y),
write(' in the '),
write(Z),
!, % cut so that it only gives one possible guess per execution.
nl,
showOptions.

% EXECUTEOPTION[2] Enters a card shown by an opponent
executeOption(2) :-
nl,
write('Which Player showed you the card (number)? '),
read(Player),
entercards(Player,1),
nl,
showOptions.

% EXECUTEOPTION[3] Tell the program you are entering or leaving a room
executeOption(3) :-
nl,
write('You are currently in the '),
%begin if-then-else
(playerRoom(X) -> write(X),
write('. Leave room [Y/N]? '),
read(Ans),
((Ans = 'Y' ; Ans = 'y') -> retract(playerRoom(_)) ; true) ;
write('corridor. Which room would you like to enter? '),
read(Room),
assert(playerRoom(Room))), % end if-then-else
showOptions.

% EXECUTEOPTION[4] Process a guess by an opponent
executeOption(4) :-
nl,
write('Which opponent made the guess (number)? '),
read(Player),
write('Enter your opponents guess in the form [person,weapon,room] : '),
read(GuessArray),
opponentGuess(GuessArray,Player),
showOptions.

% EXECUTEOPTION[5] Print to screen the remaining possible cards
executeOption(5) :- printAvailCards.

% EXECUTEOPTION[6] Clear database and exit program.
executeOption(6) :- clear.

% CLEAR - Retracts all dynamic elements
clear :- retractall(shownCard(_,_,_)), retractall(numPlayerCards(_)), retractall(numPlayers(_)), retractall(playerRoom(_)),retractall(guessCards(_)), false.

% OPPONENTGUESS - HELPER for menu item [4] - assigns each card to dynamic guessCards, then runs sub HELPER
% assignCards which assigns the card to the opponent with a probability
opponentGuess([H|T],P) :-
assert(guessCards(H)),
opponentGuess(T,P).
opponentGuess([],P) :- assignCards(P), showOptions.


% ASSIGNCARDS - HELPER for opponentGuess -
/*
cards are held by other players with a percentage <= 1.0
each card (A,B,C) is assigned
30% * products(100%-percentage of card being held by others)i, i = 1...n
e.g. (1,knife,0.3),(2,knife,0.3) -> P(knife) for player 3  = P(knife)*(0.7*0.8) = P(knife)*0.56
the prob of p1 not having is 70% the prob of p2 not having is 80%. The prob of them BOTH
not having is .7*.8. And the prob of p3 is his probability of having P(knife) *
the probability that the other players DON'T have it. */

assignCards(P) :- guessCards(X),guessCards(Y),guessCards(Z),
X \= Y, Y \= Z, X \= Z, % assign guessCards to vars
addProb(X,XP), addProb(Y,YP), addProb(Z,ZP),
assert(shownCard(P,X,XP)),
assert(shownCard(P,Y,YP)),
assert(shownCard(P,Z,ZP)).

% ADDPROB - HELPER for assignCards (card,Probability of player having)
addProb(X,Y) :- findall(P,shownCard(_,X,P),Z),addProbHelp(Z,SProb), Y is 0.30*SProb.

% ADDPROBHELP - HELPER for addProb. Product of card probs HELPER
addProbHelp([],1).
addProbHelp([H|T],Sum) :-
addProbHelp(T,Sum1),
H2 is 1 - H,
Sum is H2 * Sum1.

% PRINTAVAILCARDS - Lists all the cards that have not been shown yet
printAvailCards :- printSuspects ; printRooms ; printWeapons.
printAvailCards :- showOptions.

%PRINTSUSPECTS - HELPER for printAvailCards
printSuspects :-
nl,
write('Current Suspects: '),nl,nl,
isPerson(X),
not(shownCard(_,X,1.0)),
write(X),
nl,
fail.

%PRINTWEAPONS - HELPER for printAvailCards
printWeapons :-
nl,
write('Possible Weapons: '),nl,nl,
isWeapon(X),
not(shownCard(_,X,1.0)),
write(X),
nl,
fail.

%PRINTROOMS - HELPER for printAvailCards
printRooms :-
nl,
write('Possible Rooms: '),nl,nl,
isRoom(X),
not(shownCard(_,X,1.0)),
write(X),
nl,
fail.


/* NOTES and ideas

1. Bugs with Probability. What if player holding up cards contains a probability already.
1a. Fix Best Guess Function (should have the lowest SUM of probabilities)


2. Jessyka

Keep track of room distances from player

when you enter a room all the room values should also change based on the absolute distance from the room you are currently in. so say you have room(kitchen,1) room(study,0) room(library,2) then you go into the library… it becomes: room(library,2) room(study,1) room(kitchen,4)

*/

