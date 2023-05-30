:- module( color_map, [ color_map/0, color_map/1, test_color/2 ] ).

%:- use_module(library(util),[get_line/2,yesno/3]).
%:- use_module(library(system),[file_exists/2]).
:- use_module(library(lists),[select/3]).

/*
color_map :-
    First read the name of the file containing the map to be colored.
    After reading a set of Colors and a Map from that file
    (according to the format of map_test.pl),
    Map is colored with Colors, so that no two neighbors have the same
    color. The file contains two terms terminated by dot and white space
    characters (for the read statement), the Colors and the Map.
    The Colors are represented as a round list of color names.
    The Map is represented as a round adjacency-list of countries
    Name:Neighbors, where Name is the name of the country,
    and Neighbors is the list of the names of the neighbor countries. 
*/

% repeat.
% repeat :- repeat.

color_map :-
    repeat,
    write(user,'\nThe name of the map file (without .pl extension): '),
    flush_output(user),
    get_line(user,Cs), append(Cs,".pl",Ds), atom_codes(File,Ds),
    file_check(File),
    !,
    open(File,read,S),
    catch( ( read(S,ColorSet), read(S,Countries) ), Err,
	   (
	     ( Err = error(syntax_error(_),_) ->
	       write(user,'The form of the colors or countries is invalid.\n')
	     ; Err = error(permission_error(_,_,_),_) ->
	       write(user,
		     'The input colors and countries seem to be missing.\n')
	     ; write(user,'Unknown error in the input file.\n')
	     ),
	     close(S), throw(Err)
	   )
	 ),
    close(S),
    ( ground(ColorSet), ground(Countries) -> true
    ; write(user,'The colors and the countries '),
      write(user,'must be represented by ground terms.\n'),
      fail
    ),
    round_list_to_list(ColorSet,ColorList),
    ( countries_to_regions(Countries,Regions) -> true
    ; write(user,'The form of the countries is invalid.\n'), fail
    ),
    check_neighbors(Countries),
    sort(ColorList,Colors), length(Colors,L),
    ( L >= 4 -> print_coloring_head
    ; write(user,'At least four colors are needed.\n'), fail
    ),
    ( color_map(Regions,Colors), print_coloring(Regions),
      yesno( 'Another coloring?',
	     ( nl(user), fail ),
	     ( write(user,'Bye!\n') )
	   )
    -> true
    ; write(user,'There is no more solution.\n')
    ),
    true.
    
file_check(F) :- 
    ( absolute_file_name(F,_,[access(read),fileerrors(fail)]) -> true
    ; write(user,'The file does not exist or it is not readable.\n'),
      fail
    ).

check_neighbors(Countries) :-
    \+ ( has(Countries,X:Ys), member(Y,Ys),
	 \+ ( has(Countries,Y:Xs), member(X,Xs)
	    ; write(user,X),
	      write(user,' should be among the neighbors of '),
	      write(user,Y), write(user,'.\n'), fail
	    )
       ).

% has(Xs,Y) :- Round list Xs has the member Y.
has(X,Y) :- simple(X), !, X = Y.
has((X,Xs),Y) :- !, ( X = Y ; has(Xs,Y) ).
has(X,X).

/*
color_map(File) :-
    After reading a set of Colors and a Map from File,
    (according to the format of test_map.pl),
    Map is colored with Colors, so that no two neighbors have the same
    color. File contains two terms terminated by dot and white space
    characters (for the read statement), the Colors and the Map.
    The Colors are represented as a round list of color names.
    The Map is represented as a round adjacency-list of countries
    Name:Neighbors, where Name is the name of the country,
    and Neighbors is the list of the names of the neighbor countries. 
*/

color_map(File) :-
    open(File,read,S),
    read(S,ColorSet),
    read(S,Countries),
    close(S),
    round_list_to_list(ColorSet,Colors),
    countries_to_regions(Countries,Regions),
    color_map(Regions,Colors),
    print_colors_of_regions(Regions).

round_list_to_list((X,Xs),Zs) :- !,
    Zs = [X|Ys], round_list_to_list(Xs,Ys).
round_list_to_list(X,[X]).

countries_to_regions(Countries,Regions) :-
    countries_to_regions(Countries,_Dict,Regions).

countries_to_regions((Country,Countries),Dict,Regions) :- !,
    Regions = [ FirstRegion | OtherRegions ],
    country_to_region(Country,Dict,FirstRegion),
    countries_to_regions(Countries,Dict,OtherRegions).
countries_to_regions(Country,Dict,[Region]) :- % Country \= (_,_),
    country_to_region(Country,Dict,Region).

country_to_region( Name:NeighborNames, Dict,
		   region(Name,ColorVar,NeighborColorVars) ) :-
    lookup(Name,Dict,ColorVar),
    colorvars(NeighborNames,Dict,NeighborColorVars).

colorvars([N|Ns],D,[C|Cs]) :-
    lookup(N,D,C), colorvars(Ns,D,Cs).
colorvars([],_D,[]).

/*	
lookup(Key,Dictionary,Value) :-
    Dictionary contains Value indexed under Key,
    Dictionary is represented as a partial 
    list of pairs of the form (Key,Value)
    (the keys are supposed to be ground terms).

*/

% lookup(Key,[(Key,Value)|_Dict],Value1) :- !, Value = Value1.
% lookup(Key,[(_Key1,_Value1)|Dict],Value) :- % Key \= _Key1,
%     lookup(Key,Dict,Value).

lookup(Key,[(Key1,Value1)|Dict],Value) :-
    ( Key = Key1 -> Value = Value1
    ; lookup(Key,Dict,Value)
    ).

/*
color_map(Map,Colors) :-
    Map is colored with Colors, so that no two neighbors have the same
    color. The map is represented as an adjacency-list of regions
    region(Name,Color,Neighbors), where Name is the name of the region,
    Color is its color, and Neighbors are the colors of the neighbors. 
*/

color_map([Region|Regions],Colors) :-
    color_region(Region,Colors),
    color_map(Regions,Colors).
color_map([],_).

/*
color_region(Region,Colors) :-
    Region and its neighbors are colored using Colors so that the
    region's color is different from the color of any of its neighbors.
*/

color_region(region(_,Color,Neighbors),Colors) :-
    select(Color,Colors,Colors1),
    members(Neighbors,Colors1).

% select(X,[X|Xs],Xs).
% select(X,[Y|Ys],[Y|Zs]) :- select(X,Ys,Zs).

members([X|Xs],Ys) :- member(X,Ys), members(Xs,Ys).
members([],_).

% member(X,[X|_]).
% member(X,[_|Ys]) :- member(X,Ys).

print_coloring_head :-
    write(user,'\nThe colors of the map:\n'),
    write(user,
	  '(the rows: Country : Color / Colors of Neighbor Countries)\n\n').

print_colors_of_regions(Regions) :-
    print_coloring_head,
    print_coloring(Regions).

print_coloring([region(Name,Color,NeighborColors)|Regions]) :-
    write(user,Name), write(user,' : '), write(user,Color),
    write(user,' / '), write(user,NeighborColors), nl(user),
    print_coloring(Regions).
print_coloring([]) :- nl(user).

%	Program 14.4 developed: Map coloring

/* Test data */

test_color(Name,Map) :-
    map(Name,Map),
    colors(Name,Colors),
    color_map(Map,Colors).

map( test, [ region(a,A,[B,C,D]),     region(b,B,[A,C,E]), 
	     region(c,C,[A,B,D,E,F]), region(d,D,[A,C,F]),
	     region(e,E,[B,C,F]),     region(f,F,[C,D,E])
	   ]
   ).

map( west_europe,
     [ region(portugal,P,[E]),  region(spain,E,[F,P]),
       region(france,F,[E,I,S,B,WG,L]),  region(belgium,B,[F,H,L,WG]),
       region(holland,H,[B,WG]), region(west_germany,WG,[F,A,S,H,B,L]),
       region(luxembourg,L,[F,B,WG]), region(italy,I,[F,A,S]),
       region(switzerland,S,[F,I,A,WG]), region(austria,A,[I,S,WG])
     ]
   ).

colors(_,[red,yellow,blue,white]).

% Developed from program 14.5 of 'The Art of Prolog' by Ásványi Tibor, 2004.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% get_line(Stream,CharCodes) :-
%     Get a line from Stream. 
%     CharCodes is the list of the corresponding character codes. (ASCII)
get_line(Stream,CharCodes) :-
	( at_end_of_stream(Stream) -> CharCodes=[]
	; at_end_of_line(Stream) -> skip_line(Stream), CharCodes=[]
	; get_code(Stream,C), CharCodes=[C|Cs], get_line(Stream,Cs)
        ).

% yesno( Question, YesProgram, NoProgram ) :-
%     It types Question using write(user,Question),
%              and ' [yes] ' into the current line. 
%     If the user replies an empty line, or a line starting
%     with 'y' or 'Y', it performs 'YesProgram'.
%     If the user replies a line starting with 'n' or 'N',
%     it performs 'NoProgram'. It omits initial blanks of the input line.
yesno( Question, YesProgram, NoProgram ) :-
	write(user,Question), write(user,' [yes] '), flush_output(user),
	get_answer(Answer),
	( Answer == yes -> YesProgram
	; Answer == no -> NoProgram
	; write(user,'Wrong answer.\n'),
	    yesno( Question, YesProgram, NoProgram )
	).

% get_answer(Answer) :- Read a line from the standard input.
% If the line is empty, or its first nonblank character is y or Y, Answer=yes.
% If the first nonblank character is n or N, Answer=no.
% Otherwise Answer = the first nonblank character of this line.
get_answer(Answer) :-
	peek_code(user,C),
	( C == 0'\n -> skip_line(user), Answer = yes    % eoln
	; C == 0'Y -> skip_line(user), Answer = yes
	; C == 0'y -> skip_line(user), Answer = yes
	; C == 0'N -> skip_line(user), Answer = no
	; C == 0'n -> skip_line(user), Answer = no
	; C == (0' ) -> get_code(user,C), get_answer(Answer)
	; skip_line(user), atom_codes(A,[C]), Answer = A
	).
