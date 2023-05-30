% Main program of the game called 'Kalah'.
% The game is to be started typing '?- kalah.' at the Prolog prompt.

:- module( kalah, [ kalah/0, optimize/0 ] ).

:- use_module( alpha_beta, [ init_alpha_beta/1 ] ).
:- use_module( kalah_base, [ init_kalah/1, kalah/0 ] ).

:- init_alpha_beta(kalah_base), init_kalah(alpha_beta).

:- write( user, 'To start, type ''?- kalah.'' at the Prolog prompt.' ),
   nl(user).

optimize :-
	fcompile( [ alpha_beta, distribute_stones, kalah_utilities,
		        kalah_base, kalah
		      ]
		    ),
	use_module(kalah).

