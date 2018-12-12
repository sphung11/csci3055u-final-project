% Note: Erlang should not be used for string manipulation,
% however for demonstration purposes this is just 
% a sample program.

% This program demonstrates a basic Caesar's Shift cipher 
% by shifting characters one to the right to encrypt and
% one back to the left to decrypt.

-module(caesar_shift).
-import(io, [fwrite/1, nl/0]).
-export([main/0, encrypt/1, decrypt/1]).

main() ->
   Message = "Hello world!",
   Encrypted = encrypt(Message),
   print(Encrypted),
   Decrypted = decrypt(Encrypted),
   print(Decrypted).

% Constraint follows the ASCII table, however max is 125 
% instead of 126 since the Eshell has problems outputting 
% a tilde '~'. If condition would have to be slightly 
% modified if we are shifting more than one.
constraint(A) -> % if constraint(A, Key)
   Min = 32,
   Max = 125,
   if
      A > Max -> Min; % then -> Min + (Key - 1)
	  A < Min -> Max; % and -> Max - (Key - 1)
	  true -> A
   end.
   
encrypt(Msg) -> % if encrypt(Msg, Key)
   [ constraint(X + 1) || X <- Msg]. % then constraint(X + Key)

decrypt(Msg) -> 
   [ constraint(X - 1) || X <- Msg].

print(A) ->
   fwrite(A),
   nl().