% CPSC 312 Project 2
% Name: Edward Soo, Xuan (Annie) Li
% Student Number: 71680094, 34444109

clue :-
  setup, !, main_menu.

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

% my_char(X) is true if suspect X is being played by the user
:-dynamic my_char/1.

% played_char(X) is true if suspect X is being played as by another player
:-dynamic played_char/1.

% has_card(Player, Card) is true if Player (represented by a suspect) has Card
:-dynamic has_card/2.
:-dynamic has_one_of/2.

% envelope(Card) is true if Card is in the envelope (obviously)
:-dynamic envelope_suspect/1.
:-dynamic envelope_room/1.
:-dynamic envelope_weapon/1.

% next_player(X,Y) is true if X is the player to the immediate left of Y
:-dynamic next_player/2.

% These predicate are used to save some input and choices presented to the user
:-dynamic last_input/2.
:-dynamic last_num_choices/2.

% Retract all dynamic predicates
cleanup :-
  retractall(num_players(_)),
  retractall(my_char(_)),
  retractall(played_char(_)),
  retractall(has_card(_,_)),
  retractall(has_one_of(_,_)),
  retractall(envelope_weapon(_)),
  retractall(envelope_room(_)),
  retractall(envelope_suspect(_)),
  retractall(next_player(_,_)),
  retractall(last_input(_,_)),
  retractall(last_num_choices(_,_)).


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

read_input(At, Input) :-
  retractall(last_input(At, _)), read(Input), assert(last_input(At, Input)).

save_num_choices(At, Num_choices) :-
  retractall(last_num_choices(At, _)), assert(last_num_choices(At, Num_choices)).

select_from_list(List, Sel, At) :-
  index_zip(List, Ts, 0),
  length(List, Len),
  save_num_choices(At, Len),
  foreach(member(T, Ts), writef("%d. %d\n", T)),
  read_input(At, N), N  >= 0, N < Len, nth0(N, List, Sel).

select_from_list_none(List, Sel, At) :-
  index_zip(List, Ts, 0),
  length(List, Len),
  save_num_choices(At, Len),
  foreach(member(T, Ts), writef("%d. %d\n", T)),
  writef("%d. None of the above\n", [Len]),
  read_input(At, N), N  >= 0, N < Len, nth0(N, List, Sel).

record_num_players :-
	write('How many players? (from 2 to 6)'), nl,
	read(N), N>=2, N =< 6, assert(num_players(N)), nl;
	write('Not a valid option'),nl,nl, record_num_players.

record_my_char :-
  write('Which suspect are you playing?'), nl,
  findall(C, suspect(C), Cs),
  select_from_list(Cs, My_char, record_my_char),
  assert(my_char(My_char)), assert(played_char(My_char)), nl;
  write('Not a valid option'), nl, nl, record_my_char.

record_played_char :-
  num_players(6), foreach((suspect(C), not(my_char(C))), assert(played_char(C))).
record_played_char :-
  findall(C, played_char(C), P), num_players(Need), length(P, Need).
record_played_char :-
  findall(C, (suspect(C), not(played_char(C))), Not_played),
  write('Which are the other suspects being played?'), nl,
  select_from_list(Not_played, Played, record_played_char),
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
  record_next_player_from_list(P, No_prevs).

record_next_player_from_list(P, List) :-
  length(List, 1), member(Next, List), !, assert(next_player(Next, P)).
record_next_player_from_list(P, List) :-
  writef('Who is the player to the left of %d?', [P]), nl,
  select_from_list(List, Next, record_next_player_from_list),
  assert(next_player(Next, P)), nl;
  write('Not a valid option'), nl, nl, record_next_player_from_list(P, List).
  

record_my_cards :-
  write('What are your cards?'),nl,
  findall(C, (valid_card(C), not(has_card(_, C))), Not_mine),
  length(Not_mine, Num_choices),
  Num_choices =\= 0,
  select_from_list_none(Not_mine, Mine, record_my_cards),
  my_char(Me), !, assert(has_card(Me, Mine)), nl, record_my_cards;
  last_num_choices(record_my_cards, Num_choices), Num_choices =:= 1, nl;
  last_input(record_my_cards, N), last_num_choices(record_my_cards, Num_choices), Num_choices =:= N, nl;
  write('Not a valid option'), nl, nl, record_my_cards.

missing(Card) :-
  missing_weapon(Card);
  missing_room(Card);
  missing_suspect(Card).

missing_weapon(W) :-
  weapon(W), not(envelope_weapon(W)), not(has_card(_, W)).
missing_room(R) :-
  room(R), not(envelope_room(R)), not(has_card(_, R)).
missing_suspect(S) :-
  suspect(S), not(envelope_suspect(S)), not(has_card(_, S)).

solved :-
  envelope_weapon(What), weapon(What),
  envelope_room(Where), room(Where),
  envelope_suspect(Who), suspect(Who),
  writef("Hey, all that's left is %d, the %d, and the %d!\n", [Who, What, Where]), nl, !.

deduce_in_envelope :-
  deduce_in_envelope_weapon,
  deduce_in_envelope_room,
  deduce_in_envelope_suspect.
 
deduce_in_envelope_weapon :-
  not(envelope_weapon(_)), !;
  findall(C, missing_weapon(C), Cs),
  length(Cs, Len), Len > 1, !;
  missing_weapon(C),
  assert(envelope_weapon(C)),
  writef("Inferred that %d is the envelope weapon\n", [C]).

deduce_in_envelope_room :-
  not(envelope_room(_)), !;
  findall(C, missing_room(C), Cs),
  length(Cs, Len), Len > 1, !;
  missing_room(C),
  assert(envelope_room(C)),
  writef("Inferred that %d is the envelope room\n", [C]).
  
deduce_in_envelope_suspect :-
  not(envelope_suspect(_)), !;
  findall(C, missing_suspect(C), Cs),
  length(Cs, Len), Len > 1, !;
  missing_suspect(C),
  assert(envelope_suspect(C)),
  writef("Inferred that %d is the envelope suspect\n", [C]).

main_menu_option(print_database).
main_menu_option(record_my_suggestion).
main_menu_option(record_others_suggestion).
main_menu_option(quit).

option_function(print_database) :-
  write('So far we know:\n'),
  foreach(played_char(P), print_card_list(P)).

option_function(record_my_suggestion) :-
  i_suggested(Who, Where, What),
  i_learned(Who, Where, What).

option_function(record_others_suggestion) :-
  player_suggested(Player, Who, Where, What),
  i_observed(Player, Who, Where, What).

option_function(quit) :-
  write('Bye bye'), nl, nl.

print_card_list(P) :- 
  my_char(P),
  writef('  You as %d has the following cards:\n', [P]),
  print_suspect_list(P),
  print_room_list(P),
  print_weapon_list(P), nl.

print_card_list(P) :-
  writef('  Player %d has the following cards:\n', [P]),
  print_suspect_list(P),
  print_room_list(P),
  print_weapon_list(P), nl.

print_weapon_list(P) :- findall(Card, (has_card(P, Card), weapon(Card)), Ws), length(Ws, 0).
print_weapon_list(P) :-
  write('    Weapons:\n      '),
  foreach((has_card(P, Card), weapon(Card)), writef('%d ', [Card])), nl.

print_room_list(P) :- findall(Card, (has_card(P, Card), room(Card)), Ws), length(Ws, 0).
print_room_list(P) :-
  write('    Rooms:\n      '),
  foreach((has_card(P, Card), room(Card)), writef('%d ', [Card])), nl.

print_suspect_list(P) :- findall(Card, (has_card(P, Card), suspect(Card)), Ws), length(Ws, 0).
print_suspect_list(P) :-
  write('    Suspects:\n      '),
  foreach((has_card(P, Card), suspect(Card)), writef('%d ', [Card])), nl.

i_suggested(Who, Where, What) :-
  write('Which suspect did you suggest?'), nl,
  findall(S, suspect(S), Ss),
  select_from_list(Ss, Who, i_suggested), nl,
  write('Which weapon did you suggest?'), nl,
  findall(W, weapon(W), Ws),
  select_from_list(Ws, What, i_suggested), nl,
  write('Which room did you suggest?'), nl,
  findall(R, room(R), Rs),
  select_from_list(Rs, Where, i_suggested), !, nl;
  write('Not a valid option'), nl, nl, i_suggested(Who, Where, What).

i_learned(Who, Where, What) :-
  write('Did any other player show you a card?'), nl,
  findall(P, (played_char(P), my_char(Me), P \== Me), Others),
  select_from_list_none(Others, Player, i_learned), !, nl, nl,
  i_was_shown(Player, [Who,Where,What]), !;
  last_num_choices(i_learned, Num_choices), last_input(i_learned, N), N =:= Num_choices,
  nobody_shows_suspect(Who), nobody_shows_room(Where), nobody_shows_weapon(What), !;
  write('Not a valid option'), nl, nl, i_learn(Who, Where, What).

i_was_shown(Player, List) :-
  findall(C, (member(C, List), my_char(Me), not(has_card(Me, C))), Cards),
  writef('Which card did player %d show you?', [Player]), nl,
  select_from_list(Cards, Card, i_was_shown), nl,
  writef("%d %d\n", [Player, Card]),
  record_player_has_card(Player, Card), !;
  write('Not a valid option'), nl, nl, i_was_shown(Player, List).

record_player_has_card(_, Card) :-
  not(missing(Card)), !.
record_player_has_card(Player, Card) :-
  assert(has_card(Player, Card)).
  
nobody_shows_weapon(Card) :-
  not(missing(Card)), !;
  assert(envelope_weapon(Card)), 
  writef("Inferred that %d is the envelope weapon\n", [Card]).
  
nobody_shows_room(Card) :-
  not(missing(Card)), !;
  assert(envelope_room(Card)), 
  writef("Inferred that %d is the envelope room\n", [Card]).
  
nobody_shows_suspect(Card) :-
  not(missing(Card)), !;
  assert(envelope_suspect(Card)), 
  writef("Inferred that %d is the envelope suspect\n", [Card]).
  
player_suggested(Player, Who, Where, What) :-
  write('Which player made the suggestion?'), nl,
  findall(P, (played_char(P), my_char(Me), P \== Me), Others),
  select_from_list(Others, Player, player_suggested), nl,
  writef('Which suspect did player %d suggest?', [Player]), nl,
  findall(S, suspect(S), Ss),
  select_from_list(Ss, Who, player_suggested), nl,
  writef('Which weapon did player %d suggest?', [Player]), nl,
  findall(W, weapon(W), Ws),
  select_from_list(Ws, What, player_suggested), nl,
  writef('Which room did player %d suggest?', [Player]), nl,
  findall(R, room(R), Rs),
  select_from_list(Rs, Where, player_suggested), !, nl;
  write('Not a valid option'), nl, nl, player_suggested(Player, Who, Where, What).

i_observed(Player, Who, Where, What) :-
  writef('Did any other player show player %d a card?', [Player]), nl,
  findall(P, (played_char(P), P \== Player), Others),
  select_from_list_none(Others, Player2, i_observed), nl,
  assert(has_one_of(Player2, [Who, Where, What])), !;
  last_num_choices(i_observed, Num_choices), last_input(i_observed, N), N =:= Num_choices,
  nobody_shows_suspect(Who), nobody_shows_room(Where), nobody_shows_weapon(What), !;
  write('Not a valid option'), nl, nl, i_observed(Player, Who, Where, What).
  

main_menu :-
  deduce_in_envelope, solved;
  write('Choose an option:'), nl,
  findall(Opt, main_menu_option(Opt), Options),
  select_from_list(Options, Choice, main_menu),
  nl, option_function(Choice), nl, Choice == quit, !;
  last_input(main_menu, N), last_num_choices(main_menu, Num_choices),
  N >= 0, N < Num_choices, !, main_menu;
  write('Not a valid option'), nl, nl, !, main_menu.

