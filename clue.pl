% CPSC 312 Project 2
% Name: Edward Soo, Xuan (Annie) Li
% Student Number: 71680094, 34444109

% Game cards

character(mustard).
character(scarlet).
character(plum).
character(green).
character(white).
character(peacock).

weapon(rope).
weapon(leadpipe).
weapon(knife).
weapon(wrench).
weapon(candlestick).
weapon(pistol).

room(kitchen).
room(ballroom).
room(conservatory).
room(billiard).
room(library).
room(study).
room(hall).
room(lounge).
room(dining).

% Valid card

valid_card(X) :- character(X).
valid_card(X) :- weapon(X).
valid_card(X) :- room(X).

% Game setup

clue :- write('Welcome!'),nl.

% the number of players
:-dynamic players/1.

% my cards
:-dynamic mycards/1.

record_num_of_players :- write('How many players are there?'),nl,
	read(N), assert(player_num(N)).


record_my_cards :- write('What are your cards?'),nl,
	read(C), valid_card(C),assert(mycards(C)), record_my_cards;
	write('Not a valid card,please try again!'),nl,
	record_my_cards.

