:- module( kalah_utilities, [ lookahead/1, kalah_help/0 ] ).

:- use_module( env3, [ type_seq/1, ttyget_line/1 ] ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic lookahead/1.
 
lookahead(2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

kalah_help :- 
    repeat,
         lookahead(Level),
         type_seq( [ nl, 'Please,type', nl,
		     '  "s" to "stop" playing Kalah,', nl,
		     '  "r" to read the "rules" of the game,', nl,
		     '  "l" to change your "level" (1-4).', nl,	     
		     tab(3), Level, 
		     ' is your current level. (4 is the highest one.)', nl,
		     'Press <Enter> to continue playing Kalah.', nl,
		     'Your answer: ', flush ] ),
	ttyget_line(L), nl(user),
	( (L=[0's|_];L=[0'S|_]) -> throw(stop_kalah)
	; (L=[0'r|_];L=[0'R|_]) -> write_rules, fail
	; (L=[0'l|_];L=[0'L|_]) -> change_level, fail
	; L=[] -> true
	),
    !.

write_rules :-
    type_seq( [
'    Kalah is played on a board with two rows of six holes facing each other.',
 nl,
'Each player owns a row of six holes, plus a kalah to the right of the holes.',
 nl,
'In the initial state there are six stones in each hole and the two kalahs are'
, nl, 'empty, as follows:', nl,nl,
'                 6   6   6   6   6   6', nl,
'             0                           0', nl,
'                 6   6   6   6   6   6', nl, nl,
'    A player begins his move by picking up the stones of one of his holes.', 
 nl,
'Proceeding counterclockwise around the board, he puts one of the picked-up', 
 nl,
'stones in each hole and in his own kalah skipping the opponent''s kalah, ',
'until', nl,
'no stones remain to be distributed. There are three possible outcomes. ',
'If the', nl,
'last stone lands on the kalah, the player has another move. If the last stone'
, nl,
'lands on an empty hole owned by the player, and the opponent''s hole directly'
, nl,
'accross the board contains at least one stone, the player takes all the stones'
, nl,
'in the hole plus his last landed stone and puts them all in his kalah.', nl,
'Otherwise the player''s turn ends, and his opponent moves.', nl, nl,
'    If all the holes of a player becomes empty (even if it is not his turn to'
, nl,
'play), the stones remaining in the holes of the opponent are put into the', 
 nl,
'opponent''s kalah and the game ends. The winner of the game is the first', nl,
'player to get more than half of the stones in his kalah.', nl, nl,
'Press <Enter> to continue.', flush ] ),
    skip_line(user).


change_level :-
    repeat,
        write(user,'Your new level = '), flush_output(user),
        get(user,LevelChar), skip_line(user), Level is LevelChar-48, 
        ( 1 =< Level, Level =< 4 -> true
        ; write( user,
		 'Your new level should have been an integer between 1 and 4'),
          nl(user), fail
        ),
    !,
    retractall(lookahead(_)),
    asserta(lookahead(Level)).
