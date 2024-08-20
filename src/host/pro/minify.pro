reserved_atoms([id, main, get_reserved_atoms], [imported_from(_), reserved_atoms(_, _), singletons(_), quoted(_), quote_non_ascii(_), brace_terms(_), numbervars(_), fullstop(_), nl(_)]).
:- dynamic reserved_atoms/2.

% fonctionne pas tout à fait encore ... des problèmes avec les opérateurs arithmétiques

functors_start("abcdefghijklmnopqrstuvwxyz").
functors_cont("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789").

get_functor_from_id(Id, [F]) :-
  Id < 26,
  functors_start(FStart),
  string_chars(FStart, Chars),
  nth0(Id, Chars, F), !.

get_functor_from_id(Id, [F1 | FRest]) :-
  Idx is (Id - 26) mod 62,
  functors_cont(FCont),
  string_chars(FCont, Chars),
  nth0(Idx, Chars, F1),
  Id1 is Id // 62,
  get_functor_from_id(Id1, FRest).


get_new_value(A) :-
  b_getval(id, Id),
  Id1 is Id + 1,
  nb_setval(id, Id1),
  get_functor_from_id(Id1, F),
  reverse(F, F1),
  atom_chars(A, F1).

get_or_create(Key, Value, Bindings, Bindings) :-
  get_assoc(Key, Bindings, Value), !.
get_or_create(Key, Value, Bindings, NewBindings) :-
  repeat,
  get_new_value(Value),
  reserved_atoms(_, F),
  \+ memberchk(Value, F),
  \+ predicate_property(Value, imported_from(_)), !,
  put_assoc(Key, Bindings, Value, NewBindings).

format_term(end_of_file, _, _, _) :- halt(0), !.
format_term(A, A1, B, B1) :- format_term_aux(A, A1, B, B1).

format_term_aux(A, A, B, B) :- var(A), !.
format_term_aux(A, A1, B, B) :- atom(A), get_assoc(A, B, A1), !.
format_term_aux(A, A, B, B) :- atomic(A), !.
format_term_aux(A is A1, A is A1, B, B) :- !.
format_term_aux(T, T1, B, NewB) :-
  T =.. [F | Rest],
  (
    (predicate_property(T, imported_from(_)))->
      Value = F,
      B1 = B
      ;
      (
        memberchk(F, [':-']) ->
         Value = F, B1 = B
         ; (
           (reserved_atoms(_, Forbs), memberchk(T, Forbs)) ->
            Value = F, B1 = B
            ;get_or_create(F, Value, B, B1)
         )
      )
  ),
  foldl(format_term_aux, Rest, Rest1, B1, NewB),
  T1 =.. [Value | Rest1].

loop(B) :-
  read_term(T, []),
  format_term(T, T1, B, B1),
  numbervars(T1, 0, _, [singletons(true)]),
  write_term(T1, [quoted(true), quote_non_ascii(true), brace_terms(false), numbervars(true), fullstop(true), nl(true)]),
  loop(B1).

get_reserved_atoms :-
  read_term(T, []),
  T = reserved_atoms(_, K),
  retractall(reserved_atoms(_, _)),
  T1 = reserved_atoms([], K),
  asserta(T),
  numbervars(T1, 0, _, [singletons(true)]),
  write_term(T1, [quoted(true), quote_non_ascii(true), brace_terms(false), numbervars(true), fullstop(true), nl(true)]), !.

get_reserved_atoms :-
  retractall(reserved_atoms(_, _)),
  asserta(reserved_atoms([], [])).


main :-
  nb_setval(id, -1),
  empty_assoc(A),
  get_reserved_atoms,
  reserved_atoms(AllowedSubs, _),
  foldl(get_or_create, AllowedSubs, _, A, A1),
  loop(A1).

:- initialization((main)).
