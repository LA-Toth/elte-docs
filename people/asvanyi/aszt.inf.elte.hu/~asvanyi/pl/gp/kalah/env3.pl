
%         This module makes the usage of some commands of the 
%             SICStus Prolog environment more convenient.
%
% All the file names, commands and paths used as parameters of the following
% commands must be Prolog atoms, enclosed into single quotes, if necessary.
%
% Author:  Tibor Ásványi (asvanyi@ludens.elte.hu) 1994-2003.

:- module( env3,
	  
	   [ % Global commands of env3:
             % rc/0,rc/1, cm/0,cm/1, um/0,um/1, ul/1, fc/0,fc/1,
             % sh/0,sh/1, cd/1,cd/0,
    
             % Public predicates of env3:
	     ttyget_line/1,get_line/1,get_line/2,get/2,
	     type_seq/1,write_seq/1,write_seq/2, tab/2,
	     yesno/3, ms/2, on/2
	   ]
	 ).

:- op(540,fx,[rc,cm,um,ul,fc,sh,cd]).

:- meta_predicate yesno(+,:,:), ms(:,-).

:- multifile user:goal_expansion/3.

% The following predicates are imported from the library:
:- use_module(library(system),[/*shell/0,shell/1,working_directory/2*/]).
:- use_module(library(lists),[/*is_list/1*/]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  consult/1 and reconsult/1 updates the program from the given file(s). %%%
%%%  For a predicate that is already defined,  the original definition of  %%%
%%%  the predicate is deleted before adding the new clauses from the file. %%%
%%%  The predicates contained in the file(s) become interpreted.           %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% rc F :- (Re)consult list F of source files. ('.pl' may be omitted.)
% rc F :- (Re)consult source file F. ('.pl' must be omitted.)
%         F becomes current.
%%% user:goal_expansion( (rc F), _,
%%% 		     ( lists:is_list(F) -> reconsult(F)
%%% 		     ; reconsult(F), env3:let_current(F)
%%% 		     )
%%% 		   ).

%%% % rc :- Reconsult the current source file. (~.pl)
%%% user:goal_expansion( rc, _,
%%% 		     ( env3:current_file(F),
%%% 		       reconsult(F), env3:let_current(F)
%%% 		     )
%%% 		   ).

%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  compile/1 updates the program from the given file(s).                 %%%
%%%  For a predicate that is already defined,  the original definition of  %%%
%%%  the predicate is deleted before adding the new clauses from the file. %%%
%%%  The predicates will be compiled and cannot be listed by listing/0,1.  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% cm F :- Compile list F of source files. ('.pl' may be omitted.)
% cm F :- Compile source file F. ('.pl' must be omitted.)
%         F becomes current.
%%% user:goal_expansion( (cm F), _,
%%% 		     ( lists:is_list(F) -> compile(F) 
%%% 		     ; compile(F), env3:let_current(F)
%%% 		     )
%%% 		   ).

%%% % cm :- Compile the current source file. (~.pl)
%%% user:goal_expansion( cm, _,
%%% 		     ( env3:current_file(F),
%%% 		       compile(F), env3:let_current(F)
%%% 		     )
%%% 		   ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   fcompile/1 creates compiled ~.ql files from the given ~.pl file(s).  %%%
%%% The file can be loaded by um/0, um/1, ul/1, use_module/[1,2,3], ...    %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% fc F :- Compile list F of sources into ~.ql files. ('.pl' may be omitted.)
% fc F :- Compile source file F (~.pl) into the corresponding ~.ql file. 
%         F becomes current. (Extension '.pl' must be omitted.)
%%% user:goal_expansion( (fc F), _,
%%% 		     ( lists:is_list(F) -> fcompile(F) 
%%% 		     ; fcompile(F), env3:let_current(F)
%%% 		     )
%%% 		   ).

%%% % fc :- Compile the current source file into the corresponding ~.ql file. 
%%% user:goal_expansion( fc, _,
%%% 		     ( env3:current_file(F),
%%% 		       fcompile(F)
%%% 		     )
%%% 		   ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% use_module/1 loads the specified file(s), if it is not (they are not) %%%
%%% loaded (since its (their) last modification) into the system.         %%%
%%% If ~.ql file is available and it is newer than the corresponding ~.pl %%%
%%% file, it is loaded. Otherwise the ~.pl file is compiled incore, but   %%%
%%% if use_module/1 is called during consultation, the ~.pl file will be  %%%
%%% consulted, too. If the file is a module file, it exports all public   %%%
%%% predicates of the module file to the receiving module, except if it   %%%
%%% is overdefined. If no module is used, everything is loaded into the   %%%
%%% predefined module user, and all the procedures see each other.        %%%
%%% See the description of the module system, and of the buit-ins, too.   %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% um F :- Compile or load list F of files. ('.pl', '.ql' may be omitted.)
% um F :- Compile or load file F. ('.pl', '.ql' must be omitted.)
%         F becomes current.
%%% user:goal_expansion( (um F), _,
%%% 		     ( lists:is_list(F) -> use_module(F)
%%% 		     ; use_module(F), env3:let_current(F)
%%% 		     )
%%% 		   ).

%%% % um :- Compile or load the current file. (~.pl or ~.ql)
%%% user:goal_expansion( um, _,
%%% 		     ( env3:current_file(F),
%%% 		       use_module(F), env3:let_current(F)
%%% 		     )
%%% 		   ).

%%% % ul F :- Load or compile library file(s) F with use_module/1.
%%% %         ( '.pl', '.ql' may be omitted.) Current file is not changed.
%%% user:goal_expansion( (ul F), _, use_module(library(F)) ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Here comes a set of commands for accessing the operating system.     %%%
%%% It is strongly recommended to use  emacs  for editing and as a shell %%% 
%%% to invoke SICStus. Install command  M-x run-prolog  for convenience. %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% % sh :- Start a new Unix/Dos shell. Type exit<Return> to return to Prolog.
%%% user:goal_expansion( sh, _, system:shell ).

%%% % sh Command :- Pass Command to a new Unix/Dos shell.
%%% %               Command must be a Prolog atom.
%%% %               It should not contain Dos switches.
%%% user:goal_expansion( (sh Command), _, system:shell(Command) ).

%%% % Do not use sh/0 or sh/1 for changing the current working directory of Prolog.

%%% % cd Path :- Change the current working directory according to Path.
%%% % We suppose that no directory name has the form *.pl .
%%% user:goal_expansion( (cd Path), _,
%%%                      ( env3:abs_file_name(Path,AbsPath),
%%% 		       system:working_directory(_,AbsPath)
%%% 		     )
%%% 		   ).

%%% % cd :- Change the current working directory to my home directory.
%%% user:goal_expansion( cd, _, cd ~ ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Here come the utilities of this module. While the commands above are %%%
%%% global in the Prolog development environment, if this module is      %%%
%%% loaded, the following ones are visible only if they are explicitly   %%%
%%% or implicitly imported into the actual module of the user.           %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% get(S,C) :- get the code of the next character
%             which is greater than the blank (space) character. 
get(S,C) :-
    get_code(S,D),
    ( D > (0' ) -> C = D
    ; get(S,C)
    ).

% ttyget_line(Chars) :-
%     Get a line from the standard input. 
%     Chars is the list of the corresponding character codes. (ASCII)
ttyget_line(Chars) :- get_line(user,Chars).

% get_line(Stream,Chars) :-
%     Get a line from Stream. 
%     Chars is the list of the corresponding character codes. (ASCII)
get_line(Stream,Chars) :-
	( at_end_of_stream(Stream) -> Chars=[]
	; at_end_of_line(Stream) -> skip_line(Stream), Chars=[]
	; get_code(Stream,C), Chars=[C|Cs], get_line(Stream,Cs)
        ).

% get_line(Chars) :-
%     Get a line from the current input. 
%     Chars is the list of the corresponding character codes. (ASCII)
get_line(Chars) :-
	( at_end_of_stream -> Chars=[]
	; at_end_of_line -> skip_line, Chars=[]
	; get_code(C), Chars=[C|Cs], get_line(Cs)
        ).

% type_seq(Xs) :- 
%     Type the elements of list Xs on the standard output. 
%     Some terms are specially handled:
%     nl, flush, and tab(N)
%     raise the corresponding actions on the standard output.
type_seq(Xs) :- write_seq(user,Xs).

% write_seq(S,L) :- 
%     Write the elements of list L onto the stream S. 
%     Some terms are specially handled:
%     nl, flush, and tab(N) raise the corresponding actions on S.
write_seq(S,X) :- var(X), !, write(S,X).
write_seq(S,[X|Xs]) :- var(X), !, write(S,X), write_seq(S,Xs).
write_seq(S,[nl|Xs]) :- !, nl(S), write_seq(S,Xs).
write_seq(S,[flush|Xs]) :- !, flush_output(S), write_seq(S,Xs).
write_seq(S,[tab(N)|Xs]) :- !, tab(S,N), write_seq(S,Xs).
write_seq(S,[X|Xs]) :- !, write(S,X), write_seq(S,Xs).
write_seq(_S,[]) :- !.
write_seq(S,X) :- write(S,X).

tab(S,N) :- ( N > 0 -> put_code(S,0' ), N1 is N-1, tab(S,N1)
	    ; true
	    ).

% write_seq(Xs) :- 
%     Type the elements of list Xs on the current output. 
%     Some terms are specially handled:
%     nl, flush, and tab(N) raise the corresponding actions.
write_seq(X) :- var(X), !, write(X).
write_seq([X|Xs]) :- var(X), !, write(X), write_seq(Xs).
write_seq([nl|Xs]) :- !, nl, write_seq(Xs).
write_seq([flush|Xs]) :- !, flush_output, write_seq(Xs).
write_seq([tab(N)|Xs]) :- !, tab(N), write_seq(Xs).
write_seq([X|Xs]) :- !, write(X), write_seq(Xs).
write_seq([]) :- !.
write_seq(X) :- write(X).

% yesno( Question, YesProgram, NoProgram ) :-
%     It types Question using type_seq/1, and ' [yes] ' into the current line. 
%     If the user replies an empty line, or a line starting
%     with 'y' or 'Y', it performs 'YesProgram'.
%     If the user replies a line starting with 'n' or 'N',
%     it performs 'NoProgram'.
yesno( Question, YesProgram, NoProgram ) :-
	type_seq(Question), write(user,' [yes] '), flush_output(user),
	get_answer(Answer), 
	( Answer == 0'y -> YesProgram
	; Answer == 0'n -> NoProgram
	; write(user,'Wrong answer.'), nl(user),
	    yesno( Question, YesProgram, NoProgram )
	).

% ms(Goal,MilliSeconds) :-
%     The Prolog goal Goal needs Milliseconds milliseconds to run.
ms(Goal,MilliSeconds) :-
    statistics(runtime,_), Goal, statistics(runtime,[_,MilliSeconds]).


% on(X,Xs) :- The term X is syntactically identical
%             with one of the elements of the list Xs.
on(X,[Y|Ys]) :-
    ( X==Y -> true
    ; on(X,Ys)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Here come the local predicates of this module.

get_answer(Answer) :-
	peek_code(user,C),
	( C == 10 -> skip_line(user), Answer = 0'y    % Linux eoln
	; C == 13 -> skip_line(user), Answer = 0'y    % dos eoln
	; C == 0'Y -> skip_line(user), Answer = 0'y
	; C == 0'N -> skip_line(user), Answer = 0'n
	; C == (0' ) -> get_code(user,C), get_answer(Answer)
	; skip_line(user), Answer = C
	).

current_file(F) :- bb_get(current_file,F).

abs_file_name(F,A) :-
    absolute_file_name(F,AF),
    ( atom_concat(A,'.pl',AF) -> true
    ; A = AF
    ).

let_current(F) :-
    abs_file_name(F,A),
    bb_put(current_file,A).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% The end of module env3. %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
