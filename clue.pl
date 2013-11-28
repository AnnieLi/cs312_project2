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
:-dynamic menu_opt/1.

% next_player(X,Y) is true if X is the player to the immediate left of Y
:-dynamic next_player/2.

% Returns the index (1-based) of an element in a list, -1 if element is not in the list
index_of(X, List, -1) :- not(member(X, List)), !.
index_of(X, List, Index) :- member(X, List), !, index_of_no_fail(X, List, Index).
index_of_no_fail(X, [X|_], 1) :- !.
index_of_no_fail(X, [_|Tl], N_Index) :- index_of_no_fail(X, Tl, Index), N_Index is Index + 1.

% Returns a list of tuple {index, element} with index starting at Start
index_zip([], [], _).
index_zip([Hd|Tl], [[Start,Hd]|Tuples], Start) :- N_Start is Start + 1, index_zip(Tl, Tuples, N_Start).


% Game setup menu
record_num_of_players :- write('How many players are there?'),nl,
	read(N), N > 0, N < 7, assert(player_num(N)).

record_my_player :- write('Which character are you playing?'), nl,
  findall(C, character(C), Cs),
  index_zip(Cs, Tuples, 0),
  foreach(member(Tuple, Tuples), writef("%d. %d\n", Tuple)),
  read(N), N > -1, N < 6, nth0(N, Cs, My_C),
  assert(my_character(My_C)).

record_order :- foreach((character(C), not(next_player(_, C))), record_next_player(C)).

record_next_player(P) :- writef('Who is the player to the left of %d?', [P]), nl,
  findall(C, (character(C), not(next_player(C, _)), C \== P), No_prevs),
  length(No_prevs, Num_choice),
  index_zip(No_prevs, Tuples, 0),
  foreach(member(Tuple, Tuples), writef("%d. %d\n", Tuple)),
  read(N), N > -1, N < Num_choice, nth0(N, No_prevs, Next),
  assert(next_player(Next, P)).

record_my_cards :- write('What are your cards?'),nl,
	read(C), valid_card(C),assert(mycards(C)), record_my_cards;
	write('Not a valid card,please try again!'),nl,
	record_my_cards.

