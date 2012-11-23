
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

cardShown(Player,Card).

start :-
write('Enter the number of players: '),
read(Numplayers),
write('Enter the number of cards you recieved: '),
read(Numcards),
write('Enter one of your starting cards: '),
read(Card1),
isValidCard(Card1).

isValidCard(Card) :- isWeapon(Card) ; isRoom(Card). 

