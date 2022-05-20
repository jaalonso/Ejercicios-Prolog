% ejercicios-tema-5.pl
% Ejercicios del tema 5 (Programación lógica de orden superior).
% José A. Alonso Jiménez <https://jaalonso.github.io>
% Sevilla, 20-mayo-2022
% =============================================================================

% ------------------------------------------------------------------------------
% Ejercicio 1. Definir la relación divisores(+N,-L) que se verifique si L es
% la lista de los divisores del número N. Por ejemplo,
%    ?- divisores(24,L).
%    L = [1, 2, 3, 4, 6, 8, 12, 24].
% ------------------------------------------------------------------------------

divisores(N,L) :-
   findall(X,((between(1,N,X), N mod X =:= 0)),L).

% ------------------------------------------------------------------------------
% Ejercicio 2. En los apartados de este ejercicio se darán distintas
% definiciones de la relación primo(+N) que se verifica si N es un número
% primo. Por ejemplo,
%    ?- primo(7).
%    Yes
%    ?- primo(9).
%    No
% ------------------------------------------------------------------------------

% ------------------------------------------------------------------------------
% Ejercicio 2.1. Definir primo(X) que se verifica si el número de divisores de
% X es menor que 3 [Usar la definición anterior de divisores].
% ------------------------------------------------------------------------------

primo(X) :-
   divisores(X,L),
   length(L,N),
   N < 3.

% ------------------------------------------------------------------------------
% Ejercicio 2.2. Definir primo(X) que se verifica si no hay ningún número entre
% 2 y la raiz cuadrada de X que sea un divisor de X.
% ------------------------------------------------------------------------------

primo_2(X) :-
   Y is floor(sqrt(X)),
   not((between(2,Y,Z), X mod Z =:= 0)).

% ------------------------------------------------------------------------------
% Ejercicio 2.3. Definir primo(X) que se verifica si no hay ningún número entre
% 2 y la raiz cuadrada de X que sea primo y divida a X.
% ------------------------------------------------------------------------------

primo_3(X) :-
   Y is floor(sqrt(X)),
   not((between(2,Y,Z), primo_3(Z), X mod Z =:= 0)).

% La comparación de eficiencia es
%    ?- time(primo(15485867)).
%    % 30,971,750 inferences, 2.074 CPU in 2.075 seconds (100% CPU, 14929796 Lips)
%    true.
%
%    ?- time(primo_2(15485867)).
%    % 7,872 inferences, 0.003 CPU in 0.003 seconds (100% CPU, 2948049 Lips)
%    true.
%
%    ?- time(primo_3(15485867)).
%    % 589,106 inferences, 0.085 CPU in 0.085 seconds (100% CPU, 6947773 Lips)
%    true.

% ------------------------------------------------------------------------------
% Ejercicio 3.1. Se considera el siguiente polinomio p(X)=X^2-X+41. Definir la
% relación genera_primo(+X,+Y,-L) que se verica si L es el conjunto de los
% pares (A,p(A)) tales A es un número del intervalo [X,Y] y p(A) es primo. Por
% ejemplo,
%    ?- genera_primo(5,7,L).
%    L = [[5, 61], [6, 71], [7, 83]].
% ------------------------------------------------------------------------------

genera_primo(X,Y,L) :-
   findall([A,B],((between(X,Y,A), B is A^2-A+41, primo(B))),L).

% ------------------------------------------------------------------------------
% Ejercicio 3.2. Comprobar que p(A) es primo para A desde 1 hasta 40, pero no es
% primo para A=41.
% ------------------------------------------------------------------------------


% Solución.
%    ?- genera_primo(1,40,_L), length(_L,40).
%    true.
%
%    ?- genera_primo(41,41,L).
%    L = [].

% ------------------------------------------------------------------------------
% Ejercicio 4: Se dice que dos números X e Y son amigos cuando X es igual a la
% suma de los divisores de Y (exceptuando al propio Y) y viceversa. Por
% ejemplo, 6 es amigo de sí mismo puesto que los divisores de 6 (exceptuando al
% 6) son 1, 2 y 3 y 6=1+2+3. Igualmente 28 es amigo de sí mismo, puesto que
% 28=1+2+4+7+14.
%
% Definir la relación amigos(+A,+B,-L) que se verifique si L es la lista de las
% parejas de números amigos comprendidos entre A y B, con A menor o igual que
% B. Por ejemplo,
%    ?- amigos(1,2000,L).
%    L = [6-6, 28-28, 220-284, 496-496, 1184-1210].
% ------------------------------------------------------------------------------

amigos(A,B,L) :-
   findall(X-Y, ((between(A,B,X),
                  divisores_propios(X,Dx),
                  sumlist(Dx,Y),
                  between(X,B,Y),
                  divisores_propios(Y,Dy),
                  sumlist(Dy,X))),
           L).

divisores_propios(X,L) :-
   X1 is X-1,
   findall(N,((between(1,X1,N), X mod N =:= 0)),L).

% ------------------------------------------------------------------------------
% Ejercicio 5. Definir la relación lista_a_conjunto(+L,-C) que se verifica si C
% es el conjunto de los elementos de la lista no vacía L. Por ejemplo,
%    ?- lista_a_conjunto([b,c,d,a,c,f,a],C).
%    C = [a, b, c, d, f]
% ------------------------------------------------------------------------------

lista_a_conjunto(L,C) :-
   setof(X,member(X,L),C).

% ------------------------------------------------------------------------------
% Ejercicio 6. Definir la relación para_todos(+P,+L) que se verifica si todos
% los elementos de la lista L cumplen la propiedad P. Por ejemplo,
%    ?- para_todos(number,[1,2,3]).
%    true
%    ?- para_todos(number,[1,2,3,a]).
%    false.
% ------------------------------------------------------------------------------

para_todos(_,[]).
para_todos(P,[X|L]) :-
   apply(P,[X]),
   para_todos(P,L).

% ------------------------------------------------------------------------------
% Ejercicio 7. Definir la relación existe(+P,+L) que se verifica si existe
% algún elemento de la lista L que cumple la propiedad P. Por ejemplo,
%    ?- existe(number,[a,b,c]).
%    false.
%    ?- existe(number,[a,1,b,c]).
%    true.
% ------------------------------------------------------------------------------

existe(P,[X|_]) :-
   apply(P,[X]), !.
existe(P,[_|L]) :-
   existe(P,L).

% ------------------------------------------------------------------------------
% Ejercicio 8. Definir la relación sublista(+P,+L1,?L2) que se verifica si L2
% es la lista de los elementos de L1 que verifican la propiedad P. Por ejemplo,
%    ?- sublista(number,[1,a,2,b,1],L).
%    L = [1, 2, 1]
%
% Nota: La relación predefinida correspondiente a sublista es include.
% ------------------------------------------------------------------------------

% 1ª solución
% ===========

sublista(_P,[],[]).
sublista(P,[X|L1],[X|L2]) :-
   Q =.. [P,X],
   Q,
   sublista(P,L1,L2).
sublista(P,[X|L1],L2) :-
   Q =.. [P,X],
   \+ Q,
   sublista(P,L1,L2).

% 2ª solución
% ===========

sublista_2(_P,[],[]).
sublista_2(P,[X|L1],[X|L2]) :-
   Q =.. [P,X],
   Q, !,
   sublista_2(P,L1,L2).
sublista_2(P,[_X|L1],L2) :-
   % Q =.. [P,_X],
   % \+ Q,
   sublista_2(P,L1,L2).

% 3ª solución
% ===========

sublista_3(P,L1,L2) :-
   findall(X,(member(X,L1),apply(P,[X])),L2).

% 4ª solución
% ===========

sublista_4(_,[],[]).
sublista_4(P,[X|Xs],Ys) :-
   (   call(P, X)
   ->  Ys = [X | Zs]
   ;   Ys = Zs
   ),
   sublista_4(P, Xs, Zs).

% ------------------------------------------------------------------------------
% Ejercicio 9. Definir la relación rotaciones(L1,L2) que se verifica si L2 es la
% lista cuyos elementos son las listas formadas por las sucesivas rotaciones de
% los elementos de la lista L1. Por ejemplo,
%    ?- rotaciones([1,2,3,4],L).
%    L = [[1, 2, 3, 4], [2, 3, 4, 1], [3, 4, 1, 2], [4, 1, 2, 3]].
%    ?- rotaciones([2,3,4,1],L).
%    L = [[2, 3, 4, 1], [3, 4, 1, 2], [4, 1, 2, 3], [1, 2, 3, 4]].
% ------------------------------------------------------------------------------

% 1ª solución
% ===========

rotaciones(L1,L2) :-
   findall(L,es_rotacion(L1,L),L2).

es_rotacion(L1,L2)  :-
   append(L3,[X|L4],L1),
   append([X|L4],L3,L2).

% 2ª solución
% ===========

rotaciones_2(L1,L2) :-
   findall(L,
           (append(L3,[X|L4],L1),append([X|L4],L3,L)),
           L2).

% ------------------------------------------------------------------------------
% Ejercicio 10. Definir la relación capicúas(N,L) que se verifica si L es la
% listas de los números enteros positivos capicúa menores que N. Por ejemplo,
%    ?- capicúas(100,L).
%    L = [1, 2, 3, 4, 5, 6, 7, 8, 9|...].
%
%    ?- set_prolog_flag(answer_write_options, [quoted(true), portray(true), max_depth(100)]).
%    true.
%
%    ?- capicúas(100,L).
%    L = [1,2,3,4,5,6,7,8,9,11,22,33,44,55,66,77,88,99].
% ------------------------------------------------------------------------------

capicúas(N,L) :-
   setof(X,(between(1,N,X),es_capicúa(X)),L).

es_capicúa(N) :-
   name(N,L),
   reverse(L,L).

% ------------------------------------------------------------------------------
% Ejercicio 11. Definir la relación invierte_palabra(P1,P2) que se verifica si
% P2 es la palabra formada invirtiendo el orden de las letras de P1. Por
% ejemplo,
%    ?- invierte_palabra(arroz,P).
%    P = zorra.
% ------------------------------------------------------------------------------

invierte_palabra(P1,P2) :-
    name(P1,L1),
    reverse(L1,L2),
    name(P2,L2).
