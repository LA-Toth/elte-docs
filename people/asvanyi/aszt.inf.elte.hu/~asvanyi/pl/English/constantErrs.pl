%% Erronous constant definitions for scan.pl, d0.pl, d1,pl, d2.pl, d3.pl

A1  = a-1.      %% A1 is invalid constant name and a is undefined
b   = 3*A.      %% A is invalid operand name  
a   = 1+1.      %% valid
c3  = b//a.     %% b is undefined   
d   = b rem 4.  %% b is undefined and rem is not an operator
6 = b.          %% 6 is invalid constant symbol and b is invalid expression.
b = 9//a.       %% integer division (valid)
a = 1+d.        %% a is redefined and d is undefined
