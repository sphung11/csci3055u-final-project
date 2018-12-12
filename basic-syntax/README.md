This folder includes the sample code to illustrate
the basic syntax of the language.

Since Erlang does not allow an easy way to demonstrate the basic syntax of the language without creating an entire program, this code is only a quick simulation of the output of basic arithmetics into the Eshell.

To run the program first compile then use `sample_code:main().` from the Eshell.

```erlang
-module(sample_code).
-export([main/0]).

main() ->
   print(_ = 'Simulating Eshell Output:'),
   print(2 + 15),
   print(5 / 2),
   print(5 div 2),
   print(5 rem 2),
   print((2 + 15) - 5 / 2),
   print(2#101010),
   print(2.3e3),
   print(2.3e-3).

print(A) ->
    io:format("~p", [A]),
    io:nl().
```
