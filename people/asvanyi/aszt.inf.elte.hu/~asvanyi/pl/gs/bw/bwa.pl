/* This program solves the 'Black and White Problem'.

Goal: `11...1 22...2'

    `1' represents black, `2' represents white.
    (The position of the blank may vary.)

Initial state: Any permutation of 'Goal'.

Steps:
    - Slip a coin (black or white).
    - `black' may jump over `white' to the left.
    - `white' may jump over `black' to the right.

This program uses program `bw' with `algorithm A' (alga).
*/

:- module( bwa, [ bw/0 ] ).

% Load the modules.
:- use_module( alga, [ init_graph_search/1 ] ).
:- use_module( bw, [ init_bw/1, bw/0 ] ).

% Initialize the modules.
:- init_graph_search( bw ), init_bw( alga ).

% Start the program.
:- write(user,'\nType `bw.\' to start the program.\n'). 

