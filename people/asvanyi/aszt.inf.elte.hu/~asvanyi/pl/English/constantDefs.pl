%% Constant definitions for scan.pl, d0.pl, d1,pl, d2.pl, d3.pl

a   = 1+1. 
a_1 = a-1.   
b   = 3*a.    
c3  = b//a.       %% integer division   
d   = b mod 4.

u = c*4.    %% u depends on undefined constant (not allowed)
d = a+1.    %% d is redefined constant (not allowed)

aA = a*a.

