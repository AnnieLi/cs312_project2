% CPSC 312 Project 2
% Name: Edward Soo, Xuan (Annie) Li
% Student Number: 71680094, 34444109

clue :-
  setup, main_menu.

% Game cards
suspect(mustard).
suspect(scarlet).
suspect(plum).
suspect(green).
suspect(white).
suspect(peacock).

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
valid_card(X) :- suspect(X).
valid_card(X) :- weapon(X).
valid_card(X) :- room(X).

% the number of all players
:-dynamic num_players/1.

% played_char(X) is true if suspect is being played as by a player
:-dynamic my_char/1.
:-dynamic played_char/1.

% has_card(Player, Card) is true if Player (represented by a suspect) has Card
:-dynamic has_card/2.

% next_player(X,Y) is true if X is the player to the immediate left of Y
:-dynamic next_player/2.

% These predicate are used to save some variables
:-dynamic last_input/1.
:-dynamic last_menu_size/1.

% Returns the index (1-based) of an element in a list, -1 if element is not in the list
index_of(X, List, -1) :- not(member(X, List)), !.
index_of(X, List, Index) :- member(X, List), !, index_of_no_fail(X, List, Index).
index_of_no_fail(X, [X|_], 1) :- !.
index_of_no_fail(X, [_|Tl], N_Index) :- index_of_no_fail(X, Tl, Index), N_Index is Index + 1.

% Returns a list of tuple {index, element} with index starting at Start
index_zip([], [], _).
index_zip([Hd|Tl], [[Start,Hd]|Ts], Start) :- N_Start is Start + 1, index_zip(Tl, Ts, N_Start).


% Game setup menu
setup :-
  cleanup,record_num_players, record_my_char, record_played_char, record_order, record_my_cards, !.

read_input(Input) :-
  retractall(last_input(_)), read(Input), assert(last_input(Input)).

save_menu_size(Num_choices) :-
  retractall(last_menu_size(_)), assert(last_menu_size(Num_choices)).

cleanup :-
  retractall(num_players(_)),
  retractall(my_char(_)),
  retractall(played_char(_)),
  retractall(next_player(_,_)),
  retractall(has_card(_,_)),
  retractall(last_input(_)),
  retractall(last_menu_size(_)).

select_from_list(List, Sel) :-
  index_zip(List, Ts, 0),
  length(List, Len),
  save_menu_size(Len),
  foreach(member(T, Ts), writef("%d. %d\n", T)),
  read_input(N), N  >= 0, N < Len, nth0(N, List, Sel).

select_from_list_none(List, Sel) :-
  index_zip(List, Ts, 0),
  length(List, Len),
  save_menu_size(Len),
  foreach(member(T, Ts), writef("%d. %d\n", T)),
  writef("%d. None of the above\n", [Len]),
  read_input(N), N  >= 0, N < Len, nth0(N, List, Sel).

record_num_players :-
	write('How many players? (from 2 to 6)'), nl,
	read(N), N>=2, N<6, assert(num_players(N)), nl;
	write('Not a valid option'),nl,nl, record_num_players.

record_my_char :-
  write('Which suspect are you playing?'), nl,
  findall(C, suspect(C), Cs),
  select_from_list(Cs, My_char),
  assert(my_char(My_char)), assert(played_char(My_char)), nl;
  write('Not a valid option'), nl, nl, record_my_char.

record_played_char :-
  findall(C, played_char(C), P), num_players(Need), length(P, Need).
record_played_char :-
  findall(C, (suspect(C), not(played_char(C))), Not_played),
  write('Which are the other suspects being played?'), nl,
  select_from_list(Not_played, Played),
  assert(played_char(Played)), nl, record_played_char;
  write('Not a valid option'), nl, nl, record_played_char.

record_order :-
  findall(C, played_char(C), Played), length(Played, Num_played), Num_played =:= 2,
  foreach((played_char(C), not(next_player(_,   C))), record_order_2_players(C)), nl.
record_order :-
  foreach((played_char(C), not(next_player(_, C))), record_next_player(C)).

record_order_2_players(P) :-
  played_char(C), P \== C, assert(next_player(C, P)).
record_next_player(P) :-
  findall(C, (played_char(C), C \== P, not(next_player(C, _)), not(next_player(P, C))), No_prevs),
  length(No_prevs, Num_choices),
  Num_choices > 1,
  writef('Who is the player to the left of %d?', [P]), nl,
  select_from_list(No_prevs, Next),
  assert(next_player(Next, P)), nl;
  last_menu_size(Num_choices), Num_choices =:= 2,
  played_char(C), C \= P, not(next_player(C, _)), not(next_player(P, C)), assert(next_player(C, P));
  write('Not a valid option'), nl, nl, record_order.

record_my_cards :-
  write('What are your cards?'),nl,
  findall(C, (valid_card(C), not(has_card(_, C))), Not_mine),
  length(Not_mine, Num_choices),
  Num_choices =\= 0,
  select_from_list_none(Not_mine, Mine),
  my_char(Me), !, assert(has_card(Me, Mine)), nl, record_my_cards;
  last_menu_size(Num_choices), Num_choices =:= 1, nl;
  last_input(N), last_menu_size(Num_choices), Num_choices =:= N, nl;
  write('Not a valid option'), nl, nl, record_my_cards.

missing_weapon(W) :-
  weapon(W), not(has_card(_, W)).
missing_room(R) :-
  room(R), not(has_card(_, R)).
missing_suspect(S) :-
  suspect(S), not(has_card(_, S)).

solved :-
  findall(W, missing_weapon(W), Ws),
  length(Ws, Num_ws), Num_ws =:= 1,
  findall(R, missing_room(R), Rs),
  length(Rs, Num_rs), Num_rs =:= 1,
  findall(S, missing_suspect(S), Ss),
  length(Ss, Num_ss), Num_ss =:= 1,
  missing_weapon(What), missing_room(Where), missing_suspect(Who),
  writef("Hey, all that's left is %d, the %d, and the %d!\n", [Who, What, Where]), nl, !.

main_menu_option(print_database).
main_menu_option(record_my_action).
main_menu_option(quit).

option_function(print_database) :-
  foreach(played_char(P), print_card_list(P)).
option_function(record_my_char) :-
  write('not implemented'), nl.
option_function(quit) :-
  write('Bye bye'), nl, nl, !, clue.

print_card_list(P) :-
  writef('Player %d has the following cards:\n', [P]),
  print_suspect_list(P),
  print_room_list(P),
  print_weapon_list(P), nl.

print_weapon_list(P) :- findall(Card, (has_card(P, Card), weapon(Card)), Ws), length(Ws, 0).
print_weapon_list(P) :-
  write(' weapons:'), nl,
  foreach((has_card(P, Card), weapon(Card)), writef('   %d\n', [Card])).

print_room_list(P) :- findall(Card, (has_card(P, Card), room(Card)), Ws), length(Ws, 0).
print_room_list(P) :-
  write(' rooms:'), nl,
  foreach((has_card(P, Card), room(Card)), writef('   %d\n', [Card])).

print_suspect_list(P) :- findall(Card, (has_card(P, Card), suspect(Card)), Ws), length(Ws, 0).
print_suspect_list(P) :-
  write(' suspects:'), nl,
  foreach((has_card(P, Card), suspect(Card)), writef('   %d\n', [Card])).

i_suggested(Who, Where, What) :-
  my_char(Player), player_suggested(Player, Who, Where, What).
%player_suggested(Player, Who, Where, What).
  

main_menu :-
  solved;
  write('Choose an option:'), nl,
  findall(Opt, main_menu_option(Opt), Options),
  select_from_list(Options, Choice),
  nl, option_function(Choice), nl, main_menu;
  write('Not a valid option'), nl, nl, main_menu.

