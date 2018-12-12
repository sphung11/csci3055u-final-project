% Gets the length of the hypotenuse
% of a right angle triangle, provided
% other two lengths are given.
% Uses the pythagorean theorem:
% a^2 + b^2 = c^2

-module(lib_demo).
% import pow(A,B) and sqrt(A) from math library
-import(math,[pow/2, sqrt/1]). 
-export([pythC/2]).

pythC(A,B) ->
   sqrt(pow(A,2) + pow(B,2)).