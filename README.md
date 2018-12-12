# Erlang

- Stephanie Phung
- stephanie.phung1@uoit.net

## About the language

Erlang, aka Erlang/OTP or OTP, is a general-purpose, concurrent, functional programming language, with a garbage-collected runtime system. The language was created in 1986, and was originally a proprietary language within the Swedish multinational networking and telecommunications company, Ericsson. It was released as open source in 1998, and is still being supported and maintained by the company.

Erlang designs are well suited for systems that are:
- Distributed
- Fault-tolerant
- Soft real-time
- Highly available, non-stop applications
- Hot swapping

Properties of Erlang include:
- Immutable data
- Pattern matching
- Functional programming

## About the syntax

In the Erlang shell, expressions must be terminated with a period followed by whitespace. This syntax comes from the days when Erlang was implemented directly in Prolog, which is a logic programming language.

### Arithmetic ###

Both floating point numbers and integers are supported when dealing with arithmetic. For integer-to-integer division, use `div`, and for modulo use `rem`. Mathematical operations obey normal precedence rules. Comments begin with `%` and continues until the end of the line. Integers can be expressed in other bases, following the syntax `Base#Value`.

```erlang
 2 + 15. % 17
 5 / 2. % 2.5
 5 div 2. % 2
 5 rem 2. % 1
 (2 + 15) - 5 / 2. % 14.5
 2#101010. % 42
 2.3e3. % 2.3e3
 2.3e-3. % 0.0023
 ```

### Variables ###

Since Erlang is a functional programming language, variables can only be assigned once. To erase the variable, we can use `f(Variable)`, and to clear all variables, we can use `f()`. There is a special variable `_` that does not store the value. Note that variables must begin with an uppercase letter. 

```erlang
 One.
 % * 1: variable 'One' is unbound
 One = 1.
 % 1
 Un = Uno = One = 1.
 % 1
 Uno = Uno + 1.
 % ** exception error: no match of right hand side value 2
 _ = 42.
 % 42
 _.
 % * 1: variable '_' is unbound
 f(One).
 % ok
 f().
 % ok
 Un.
 % * 1: variable 'Un' is unbound
```

### Atoms ###

Atoms are literals, constants with their own name for a value. The begin with lowercase characters. Atoms should be enclosed in single quotes if they do not begin with a lower-case letter or if it contains any character other than alphanumeric, `_` and `@` characters. Note that every atom consumes memory and is stored in an atom table, which is not garbage collected. They should never be generated dynamically. Some atoms are reserved words: `after and andalso band begin bnot bor bsl bsr bxor case catch cond div end fun if let not of or orelse query receive rem try when xor`.

```erlang
 cat. % cat
 i_am_robot. % i_am_robot
 hello@world. % hello@world
 'Adios!'. % 'Adios'
```

### Boolean Operators ###

Erlang uses boolean operators like all other programming languages. The difference is that arguments on both side of the operator will be evaluated, unless you use short-circuit operators (such as `andalso, orelse`), which will only evaluate the right-side if necessary. Do note that true or false are not actually booleans, but in fact atoms. Syntax for comparisons are also slightly different. 

*Boolean Operators*
```erlang
 false or true. % true
 true xor false. % true
 not (true and true). % false
 5 =:= 5. % true (==)
 1 =/= 0. % true (!=)
 5 =:= 5.0. % false
 5 == 5.0. % true
 5 /= 5.0. % false 
 1 < 2. % true
 1 >= 1. % true
 1 =< 1. % true
```

### Lists ###

Finally, lists must be mentioned as they are at the core of functional programming. In Erlang, lists can contain anything. You can mix more than one type of data as well. However, Strings are lists, and the notation is exactly the same. Erlang will print lists of numbers as numbers only when at least one of them cannot represent a letter; there is no such thing as a real string in Erlang. You can glue lists together or remove elements by using `++` and `--`; These operators are right-associative, which means the operations are done from right to left. The first element in a list is named the Head, and the rest of the list is the Tail. You can access these with `hd([])` and `tl([])`, or by using pattern matching `[Head|Tail]`.

```erlang
 [1, 2, 3, {numbers, [4, 5, 6]}, 5.34, atom].
 % [1,2,3,{numbers,[4,5,6]},5.34,atom]
 [97, 98, 99]. % "abc"
 [97, 98, 99, 4, 5, 6]. % [97,98,99,4,5,6]
 [51]. % 3
 [1,2,3] ++ [4,5]. % [1,2,3,4,5]
 [1,2,3,4,5] -- [1,2,3]. % [4,5]
 [97,98,99]--[99]--[98]. % "ab"
 ([97,98,99]--[99])--[98]. % "a"
 hd([1,2,3,4]). % 1
 tl([1,2,3,4]). % [2,3,4]
 List = [2,3,4]. % [2,3,4]
 NewList = [1|List]. % [1,2,3,4]
 [Head|Tail] = NewList. % [1,2,3,4]
 Head. % 1
 Tail. % [2,3,4]
```

## About the tools

All functions in Erlang must be defined in modules. You can also add attributes using `-Tag(Value)`. The module and export function must be included where the syntax is `-export([funcName/funcNumImputs , ...])`.

```erlang
 -module(demo_name). % file name is 'demo_name.erl'
 -author("Stephanie Phung"). % this is an attribute
 -version("1.0"). % this is an attribute
 -export([hello/0, add/2]). % export function
 
 hello() ->
    io:fwrite("Hello, world!\n").
 
 add(A,B) ->
    A + B.
```

We can run Erlang scripts manually using the command prompt (cmd), or using an IDE like Eclipse or IntelliJ. You can also run code directly on the Eshell. If using cmd, you must first compile the .erl file to create a .beam object code file. To compile the above sample program and run the start function, do:

```erlang
 c(demo_name). 
 % {ok,demo_name}

 demo_name:hello().
 % Hello, world!
 % ok
```

## About the standard library

There are many built in libraries that Erlang supports, including array, io, maps, math, string , and timer, among others. It is not possible to import all functions of a library, you must do so manually for the ones you need. For example, the math library exports the functions `cos(X) -> float(), log(X) -> float(), pow(X, Y) -> float(), and sqrt(X) -> float()`. If we wanted to import the log and pow functions, we would have to include `-import(library, [funcName/funcNumInputs , ...])` at the top of our module. Then we can call the imported functions by their name.

```erlang
 -module(import_demo).
 -import(math,[log/1, pow/2]).
 -export([main/0]).
 main() -> log(10). % 2.302585..
```

## About open source library

Erlang has many open source libraries available. A shortlist can be found linked below. Among this list, the OAuth2 library is designed to simplify implementation of the server side authorization framework which enables a third-party application to obtain limited access to an HTTP service.

Shortlist: https://github.com/drobakowski/awesome-erlang

OAuth2: https://github.com/kivra/oauth2

# Analysis of the language

_(Q6)_ Erlang is dynamically typed, which means that errors are only caught at runtime, not before. Despite how static type systems are seen as safer than dynamic systems, Erlang has a great track record for being one of the safest and most stable languages. This is partly due to the fact that Erlang is built on the notion that a failure on one component should not affect the rest of the system. The language includes features that allow distribution of a program over different nodes, handle unexpected errors, and nearly flawless uptime. Erlang is also strongly typed, which means it requires explicit type conversions.

_(Q1,7)_ Since Erlang is a functional programming language, it brings similar strengths and weaknesses as other functional languages. Programmers who are used to procedural or OOP will have a harder time learning functional languages. As mentioned before, code organizing is limited to modules, unlike other languages which also offer namespaces and packages. Additionally, Strings are implemented as array of integers, which can be a hassle to work around. There are also less Erlang developers, which means documentation and sample code is less detailed and harder to find. The biggest weakness is the limited amount of debugging tools available, compared to other languages. The minimal stacktrace presented makes it difficult to fix bugs. Erlang is also not good at processing large blocks of data. On the other hand, the biggest strength of Erlang is a yearly up-time of 99.999%, due to its safety features, such as horizontal and vertical scalability, code hot-swapping, asynchronous, resource management, and etc. This is why companies such as Amazon, Yahoo!, Facebook, and WhatsApp use Erlang to provide database and chat services, for example. 

_(Q2,3,4,5)_ In terms of additional programming features, Erlang offers anonymous functions and macros, as well as pattern matching. Concurrency, multiprocessing, errors, and event handlers are also easy to manage in this language. Because of the way Erlang is built, scopes are restricted to within functions only. Inside functions, there can be smaller scopes from conditional statements, such as `if, case, loop, send, receive, try, catch`, and so on. Parentheses can also indicate scope for arithmetic `()`, and blocks can be created with `begin`. Most, if not all, of these statements end at the atom `end`. 
