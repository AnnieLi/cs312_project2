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

% my cards
:-dynamic mycards/1.

% played_char(X) is true if character is being played as by a player
:-dynamic my_char/1.
:-dynamic played_char/1.

% next_player(X,Y) is true if X is the player to the immediate left of Y
:-dynamic next_player/2.
% This predicate is used to save the last user input
:-dynamic last_input/1.

% Returns the index (1-based) of an element in a list, -1 if element is not in the list
index_of(X, List, -1) :- not(member(X, List)), !.
index_of(X, List, Index) :- member(X, List), !, index_of_no_fail(X, List, Index).
index_of_no_fail(X, [X|_], 1) :- !.
index_of_no_fail(X, [_|Tl], N_Index) :- index_of_no_fail(X, Tl, Index), N_Index is Index + 1.

% Returns a list of tuple {index, element} with index starting at Start
index_zip([], [], _).
index_zip([Hd|Tl], [[Start,Hd]|Ts], Start) :- N_Start is Start + 1, index_zip(Tl, Ts, N_Start).


% Game setup menu
setup :- cleanup, record_my_char, record_played_char, record_order.

read_input(Input) :- retractall(last_input(_)), read(Input), assert(last_input(Input)).

cleanup :- retractall(played_char(_)),
  retractall(next_player(_,_)),
  retractall(my_char(_)),
  retractall(last_input(_)).

record_my_char :- write('Which character are you playing?'), nl,
  findall(C, character(C), Cs),
  index_zip(Cs, Ts, 0),
  foreach(member(T, Ts), writef("%d. %d\n", T)),
  read(N), N > -1, N < 6, nth0(N, Cs, My_C),
  assert(my_char(My_C)), assert(played_char(My_C));
  write('Not a valid option'), nl, record_my_char.

record_played_char :- write('Which other character is being played?'), nl,
  findall(C, (character(C), not(played_char(C))), Not_played),
  length(Not_played, Num_choices),
  index_zip(Not_played, Ts, 0),
  foreach(member(T, Ts), writef("%d. %d\n", T)),
  writef("%d. None of the above\n", [Num_choices]),
  read_input(N), N > -1, N < Num_choices, nth0(N, Not_played, Played),
  assert(played_char(Played)), record_played_char;
  last_input(N), !, findall(C, (character(C), not(played_char(C))), Not_played),
  length(Not_played, Num_choices), Num_choices =:= N;
  write('Not a valid option'), nl, record_played_char.

record_order :- foreach((played_char(C), not(next_player(_, C))), record_next_player(C)).
record_next_player(P) :- findall(C, (played_char(C), not(next_player(C, _)), C \== P), No_prevs),
  length(No_prevs, Num_choices),
  Num_choices =\= 1,
  writef('Who is the player to the left of %d?', [P]), nl,
  index_zip(No_prevs, Ts, 0),
  foreach(member(T, Ts), writef("%d. %d\n", T)),
  read(N), N > -1, N < Num_choices, nth0(N, No_prevs, Next),
  assert(next_player(Next, P));
  findall(C, (played_char(C), not(next_player(C, _)), C \== P), No_prevs),
  length(No_prevs, Num_choices), Num_choices =:= 1,
  played_char(C), not(next_player(C, _)), !, assert(next_player(C, P));
  write('Not a valid option'), nl, record_order.

record_my_cards :- write('What are your cards?'),nl,
	read(C), valid_card(C),assert(mycards(C)), record_my_cards;
	write('Not a valid card,please try again!'),nl,
	record_my_cards.

