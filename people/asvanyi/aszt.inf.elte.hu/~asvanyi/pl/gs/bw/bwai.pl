/* This program solves the 'Black and White Problem'.

Goal: 'bb...b_ww...w'

    'b' represents black, 'w' represents white.
    The position of the blank represented by '_' may vary.)

Initial state: Any permutation of 'Goal'.

Steps:
    - Slip a coin (black or white).
    - `black' may jump over `white' to the left.
    - `white' may jump over `black' to the right.

This program uses program `bwi' with `algorithm A' (alga).
*/

:- module( bwai, [ bw/0 ] ).

% Load the modules.
:- use_module( alga, [ init_graph_search/1 ] ).
:- use_module( bwi, [ init_bw/1, bw/0 ] ).

% Initialize the modules.
:- init_graph_search( bwi ), init_bw( alga ).

% Start the program.
:- write(user,'\nType `bw.'' to start the program.\n').
