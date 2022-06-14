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

% ----------------------------------------------------------------------
% Ejercicio 4.1. Definir la relación divisores_propios(+N,-L) que se
% verifique si L es la lista ordenada de los divisores propios del
% número N. Por ejemplo,
%    ?- divisores_propios(42,L).
%    L = [1, 2, 3, 6, 7, 14, 21] 
% ----------------------------------------------------------------------

divisores_propios(N,L) :-
   N1 is N -1,
   findall(X,(between(1,N1,X), 0 =:= N mod X),L).

% ------------------------------------------------------------------------------
% Ejercicio 4.2 Se dice que dos números X e Y son amigos cuando X es igual a la
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

% ----------------------------------------------------------------------
% Ejercicio 12. Definir la relación factorial_inverso(+X,-N) que se
% verifique si X es el factorial de N. Por ejemplo,
%    ?- factorial_inverso(120,N).
%    N = 5 ;
%    false.
%    ?- factorial_inverso(80,N).
%    false.
% ----------------------------------------------------------------------

% 1ª solución
% ===========

factorial_inverso_1(X,N) :-
   factorial_inverso_1_aux(X,1,N).

% factorial_inverso_1_aux(+X,+A,-N) se verifica si N es el menor número
% mayor o igual que A cuyo factorial es X.
factorial_inverso_1_aux(X,A,A) :-
   factorial(A,X).
factorial_inverso_1_aux(X,A,N) :-
   factorial(A,N1),
   N1 < X,
   A1 is A + 1,
   factorial_inverso_1_aux(X,A1,N).

% factorial(+N,-X) se verifica si X es el factorial de N. 
factorial(1,1).
factorial(N,X) :-
   N > 1,
   N1 is N-1,
   factorial(N1,X1),
   X is X1 * N.

% 2ª solución (con memorización)
% ==============================

factorial_inverso_2(X,N) :-
   factorial_inverso_2_aux(X,1,N).

% factorial_inverso_2_aux(+X,+A,-N) se verifica si N es el menor número
% mayor o igual que A cuyo factorial (con memoria) es X.
factorial_inverso_2_aux(X,A,A) :-
   factorial_con_memoria(A,X).
factorial_inverso_2_aux(X,A,N) :-
   factorial_con_memoria(A,N1),
   N1 < X,
   C1 is A + 1,
   factorial_inverso_2_aux(X,C1,N).

% factorial_con_memoria(+N,-X) se verifica si X es el factorial de
% N. Además almacena en la base de datos internas los factoriales
% calculados.
:- dynamic factorial_con_memoria/2.

factorial_con_memoria(1,1).
factorial_con_memoria(N,X) :-
   N > 1,
   N1 is N-1,
   factorial_con_memoria(N1,X1),
   X is X1 * N,
   asserta(factorial_con_memoria(N,X) :- !).

% 3ª solución (con dos acumuladores)
% ==================================

factorial_inverso_3(X,N) :-
   factorial_inverso_aux_3(X,1,1,N).

% factorial_inverso_aux_3(+X,+A,+F,-N) se verifica si X = A*(A+1)*...*N
% (de forma que si A = 1 entonces X = N!).
factorial_inverso_aux_3(X,A,F,A) :-
   A*F =:= X.
factorial_inverso_aux_3(X,A,F,N) :-
   F1 is A*F,
   F1 < X, !,
   A1 is A + 1,
   factorial_inverso_aux_3(X,A1,F1,N).

% Comparación de eficiencia
%    ?- factorial(2000,_X), time(factorial_inverso_1(_X,_N)).
%    % 11,997,999 inferences, 2.921 CPU in 2.925 seconds (100% CPU, 4106828 Lips)
%    true 
%    ?- factorial(2000,_X), time(factorial_inverso_2(_X,_N)).
%    % 13,993 inferences, 0.004 CPU in 0.004 seconds (100% CPU, 3832958 Lips)
%    true 
%    ?- factorial(2000,_X), time(factorial_inverso_3(_X,_N)).
%    % 7,997 inferences, 0.009 CPU in 0.009 seconds (100% CPU, 877368 Lips)
%    true 
%    
%    ?- factorial(4000,_X), time(factorial_inverso_2(_X,_N)).
%    % 35,993 inferences, 0.035 CPU in 0.035 seconds (100% CPU, 1035996 Lips)
%    true 
%    ?- factorial(4000,_X), time(factorial_inverso_3(_X,_N)).
%    % 15,997 inferences, 0.022 CPU in 0.022 seconds (100% CPU, 716325 Lips)
%    true 

% ----------------------------------------------------------------------
% Ejercicio 13. Un árbol binario es vacío o consta de tres partes: la
% raíz (que debe de ser un número positivo), el subárbol izquierdo (que
% debe ser un árbol binario) y el subárbol derecho (que debe ser un
% árbol binario). Usaremos la siguiente representación
% + nil representa el árbol vacío
% + t(I,R,D) representa el árbol de la raíz R, subárbol izquierdo I y
%   subárbol derecho D.
% Por ejemplo, t(t(nil,2,nil),1,t(t(nil,4,nil),3,nil)) representa el árbol
%       1                                   
%     /   \                                 
%    2     3                                
%         /                                 
%        4                                  
% 
% Definir la relación generación(+N,+L1,-L2) que se verifique si L2 es
% la lista de nodos de la generación N de la lista de árboles L1. Por
% ejemplo,
%    ?- generación(0,[t(t(nil,2,nil),3,nil),t(nil,4,t(nil,5,nil))],L).
%    L = [3, 4] 
%    ?- generación(1,[t(t(nil,2,nil),3,nil),t(nil,4,t(nil,5,nil))],L).
%    L = [2, 5] 
%    ?- generación(2,[t(t(nil,2,nil),3,nil),t(nil,4,t(nil,5,nil))],L).
%    L = [] 
% ----------------------------------------------------------------------

generación(0,L,G):-
   findall(R,member(t(_,R,_),L),G).
generación(N,L,G):-
   N > 0,
   elimina_raices(L,L1),
   N1 is N-1,
   generación(N1,L1,G).

% elimina_raices(+L1,-L2) se verifica si L2 es la lista de los árboles
% obtenidos de la lista de árboles L1 eliminando sus raices. Por
% ejemplo,
%    ?- elimina_raices([t(t(nil,2,nil),3,nil),t(nil,4,t(nil,5,nil))],L).
%    L = [t(nil, 2, nil), nil, nil, t(nil, 5, nil)] 
elimina_raices([],[]).
elimina_raices([nil|L1],L2):-
   elimina_raices(L1,L2).
elimina_raices([t(I,_,D)|L1],[I,D|L2]):-
   elimina_raices(L1,L2).

% ----------------------------------------------------------------------
% Ejercicio 14. Definir la relación únicos(+L1,-L2) que se verifique si
% L2 es la lista de los elementos que ocurren solamente una vez en la
% lista L1. Por ejemplo,
%    ?- únicos([2,5,3,2],L).
%    L = [5, 3] 
%    ?- únicos([2,5,5,2],L).
%    L = [] 
% ----------------------------------------------------------------------

únicos(L1,L2) :-
   findall(X,es_único(X,L1),L2).

% es_único(?X,+L) se verifica si X es un elemento que ocurre solamente
% una vez en la lista L. Por ejemplo,
%    ?- es_único(X,[2,5,3,2]).
%    X = 5 ;
%    X = 3 ;
%    false.
es_único(X,L) :-
   select(X,L,R),
   not(memberchk(X,R)).

% ----------------------------------------------------------------------
% Ejercicio 15. Definir el predicado populares(L1,L2) que se verifique
% si L2 es la lista de los elementos de L1 que aparecen el mayor
% número de veces. Por ejemplo,
%    ?- populares([rosa,juan,david,manu,rosa,nuria,david],L).
%    L = [david,rosa]
% ----------------------------------------------------------------------

populares(L1,L2) :-
   setof(X,
         ((member(X,L1),
           cuenta(X,L1,N1),
           not((member(Y,L1),
                cuenta(Y,L1,N2),
                N1 < N2)))),
         L2).

% cuenta(+X,+L,-N) se verifica si N es el número de veces que aparece el
% elemento X en la lista L. Por ejemplo,
%    ?- cuenta(d,[r,j,d,m,r,n,d],N).
%    N = 2
cuenta(_,[],0).
cuenta(A,[B|L],N) :-
   A == B, !,
   cuenta(A,L,M),
   N is M+1.
cuenta(A,[_B|L],N) :-
   % A \== _B,
   cuenta(A,L,N).

% ----------------------------------------------------------------------
% Ejercicio 16.1. Consideremos la función siguiente definida sobre los
% números naturales: 
% f(x) = 3x + 1, si x es impar;
%        x / 2,  si x es par
% 
% Definir la relación sucesión(+X,?L) que se verifique si L es la lista
% de los elementos X, f(X), f(f(X)), ..., f^n(X) tal que f^n(X) = 1. Por
% ejemplo,
%    ?- sucesión(3,L).
%    L = [3, 10, 5, 16, 8, 4, 2, 1]
% 
% L se llama la sucesión generada por X.
% ----------------------------------------------------------------------

f(X,Y) :-
   X mod 2 =:= 0, !,
   Y is X/2.
f(X,Y) :-
   % X mod 2 =/= 0,
   Y is 3*X+1.

sucesión(1,[1]) :- !.
sucesión(X,[X|L]) :-
   % X =/= 1,
   f(X,Y),
   sucesión(Y,L).

% ----------------------------------------------------------------------
% Ejercicio 16.2. Definir la relación longitudes(+X,?L) que se verifica
% si L la lista de pares Y-N donde Y es un número de 1 a X y N es la
% longitud de sucesión generada por Y. Por ejemplo,
%    ?- longitudes(5,L).
%    L = [1-1, 2-2, 3-8, 4-3, 5-6] 
% ----------------------------------------------------------------------

% 1ª solución
% ===========

longitudes(X,L) :-
   longitudes_aux(X,L1),
   reverse(L1,L).
longitudes_aux(1,[1-N]) :-
   !,
   sucesión(1,L),
   length(L,N).
longitudes_aux(X,[X-N|L]) :-
   % X > 1,
   sucesión(X,L1),
   length(L1,N),
   Y is X-1,
   longitudes_aux(Y,L).

% 2ª solución
% ===========

longitudes_2(X,L) :-
   findall(Y-N,(between(1,X,Y),sucesión(Y,S),length(S,N)),L).

% ----------------------------------------------------------------------
% Ejercicio 16.3. Definir la relación longitud_máx(+X,?P) que se
% verifica si P es un par de la forma Y-N donde Y es un número entre 1 y
% X tal que la longitud de la sucesión generada por Y es la más larga de
% las sucesiones generadas por 1, 2, ..., X y N es la longitud de dicha
% sucesión. Por ejemplo,
%    ?- longitud_máx(10,L).
%    L = 9-20
% ----------------------------------------------------------------------

longitud_máx(X,Y-N) :-
   longitudes(X,L),
   member(Y-N,L),
   \+ (member(_Z-M,L), M > N).

% ----------------------------------------------------------------------
% Ejercicio 16.4. Definir menor_que_genera_mayor(+N,-M) que se verifique
% si M es el menor número natural tal que la longitud de la sucesión
% generada por M es mayor que N. Por ejemplo,
%    ?- menor_que_genera_mayor(100,N).
%    N = 27.
% ----------------------------------------------------------------------

menor_que_genera_mayor(N,M) :-
   menor_que_genera_mayor_aux(N,1,M).
menor_que_genera_mayor_aux(N,M,M) :-
   sucesión(M,L),
   length(L,X),
   X > N, !.
menor_que_genera_mayor_aux(N,X,M) :-
   Y is X+1,
   menor_que_genera_mayor_aux(N,Y,M).

% ----------------------------------------------------------------------
% Ejercicio 17.1. Definir la relación suma_divisores_propios(+N,-S) que
% se verifique si S es la suma de los divisores propios del número
% N. Por ejemplo,
%    ?- suma_divisores_propios(42,S).o
%    S = 54.
%    ?- suma_divisores_propios(1,S).
%    S = 0. 
% ----------------------------------------------------------------------

suma_divisores_propios(N,S) :-
   divisores_propios(N,L),
   sum_list(L,S).

% ----------------------------------------------------------------------
% Ejercicio 17.2. Clasificamos los números naturales en tres tipos:
% + N es de tipo a si N es mayor que la suma de sus divisores propios
% + N es de tipo b si N es igual que la suma de sus divisores propios
% + N es de tipo c si N es menor que la suma de sus divisores propios
% 
% Definir la relación tipo(+N,-T) que se verifique si T es el tipo del
% número N. Por ejemplo,
%    ?- tipo(10,T).
%    T = a 
%    ?- tipo(28,T).
%    T = b 
%    ?- tipo(12,T).
%    T = c 
% ----------------------------------------------------------------------

% 1ª solución
% ===========

tipo(N,T) :-
   suma_divisores_propios(N,S),
   tipo_aux(N,S,T).

tipo_aux(N,S,a) :- N > S, !. 
tipo_aux(N,N,b) :- !.
tipo_aux(_N,_S,c). % :- _N < _S.

% 2ª solución
% ===========

tipo_2(N,T) :-
   suma_divisores_propios(N,S),
   ( N > S   -> T = a ;
     N =:= S -> T = b ;
     true    -> T = c).

% ----------------------------------------------------------------------
% Ejercicio 17.3. Definir la relación clasifica(+N,-L) que se verifique
% si L es la lista de tipos de los números comprendidos entre 1 y N. Por
% ejemplo, 
%   ?- clasifica(20,L).
%   L = [a, a, a, a, a, b, a, a, a, a, a, c, a, a, a, a, a, c, a, c] 
% ----------------------------------------------------------------------

% 1ª solución
% ===========

clasifica(N,L) :-
   findall(T,(between(1,N,X),tipo(X,T)),L).

% 2ª solución
% ===========

clasifica_2(N,L) :-
   numlist(1,N,L1),
   maplist(tipo,L1,L).

% ----------------------------------------------------------------------
% Ejercicio 17.4. Definir la relación promedio(+N,-A,-B,-C) que se
% verifique si A, B y C son las cantidades de números naturales menores
% o iguales que N de tipo a, b y c, respectivamente. Por ejemplo,
%    ?- promedio(20,A,B,C).
%    A = 16,
%    B = 1,
%    C = 3.
% ----------------------------------------------------------------------

promedio(N,A,B,C) :-
   clasifica(N,L),
   promedio_aux(L,A,B,C).

promedio_aux([],0,0,0).
promedio_aux([a|L],A1,B,C) :-
   promedio_aux(L,A,B,C),
   A1 is A+1.
promedio_aux([b|L],A,B1,C) :-
   promedio_aux(L,A,B,C),
   B1 is B+1.
promedio_aux([c|L],A,B,C1) :-
   promedio_aux(L,A,B,C),
   C1 is C+1.

% ----------------------------------------------------------------------
% Ejercicio 17.5. Definir la relación menor(+N,-X) que se verifique si X
% es el menor número tal que la cantidad de números naturales menores o
% iguales que X de tipo a es N. Por ejemplo,
%    ?- menor(20,X).
%    X = 25.
% ----------------------------------------------------------------------

menor(N,X) :-
   menor_aux(N,N,X).

menor_aux(N,M,M) :-
   promedio(M,N,_,_), !.
menor_aux(N,M,X) :-
   M1 is M+1,
   menor_aux(N,M1,X).

% ----------------------------------------------------------------------
% Ejercicio 18. Definir la relación operación_listas(+O,+L1,+L2,-L3) que
% se verifica si L3 es la lista obtenida aplicando la operación binaria
% O a los elementos de L1 y L2 que ocupan la misma posición (Se supone
% que L1 y L2 tienen la misma longitud). Por ejemplo, 
%    ?- operación_lista(+,[1,2,3],[4,5,6],L).
%    L = [5, 7, 9] 
%    ?- operación_lista(*,[1,2,3],[4,5,6],L).
%    L = [4, 10, 18] 
% ----------------------------------------------------------------------

operación_lista(_,[],[],[]).
operación_lista(O,[X1|L1],[X2|L2],[X3|L3]) :-
   E =.. [O,X1,X2],
   X3 is E,
   operación_lista(O,L1,L2,L3).

   
