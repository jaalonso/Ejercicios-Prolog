% ejercicios-tema-4.pl
% Ejercicios del tema 4 (Retroceso, corte y negación)
% José A. Alonso Jiménez <https://jaalonso.github.io>
% Sevilla, 20-mayo-2022
% =============================================================================

% ------------------------------------------------------------------------------
% Ejercicio 1. Definir la relación diferencia(C1,C2,C3) que se verifica si C3 es
% la diferencia de los conjuntos C1 y C2. Por ejemplo,
%    ?- diferencia([a,b],[b,c],X).
%    X = [a] ;
%    false.
%
% Hacer una versión con not y otra con corte.
% ------------------------------------------------------------------------------

% 1ª solución (con not)
% =====================

diferencia_1([],_,[]).
diferencia_1([X|C1],C2,C3) :-
   member(X,C2),
   diferencia_1(C1,C2,C3).
diferencia_1([X|C1],C2,[X|C3]) :-
   not(member(X,C2)),
   diferencia_1(C1,C2,C3).

% 2ª solución (con corte)
% =======================

diferencia_2([],_,[]).
diferencia_2([X|C1],C2,C3) :-
   member(X,C2), !,
   diferencia_2(C1,C2,C3).
diferencia_2([X|C1],C2,[X|C3]) :-
   % not(member(X,C2)),
   diferencia_2(C1,C2,C3).

% ------------------------------------------------------------------------------
% Ejercicio 2: Definir la relación agregar(X,L,L1) que se verifica si L1 es la
% lista obtenida añadiéndole X a L, si X no pertenece a L y es L en caso
% contrario. Por ejemplo,
%    ?- agregar(a,[b,c],L).
%    L = [a, b, c]
%    ?- agregar(b,[b,c],L).
%    L = [b, c]
% ------------------------------------------------------------------------------

% 1ª solución
% ===========

agregar_1(X,L,[X|L]) :-
   not(member(X,L)).
agregar_1(X,L,L) :-
   member(X,L).

% 2ª solución
% ===========

agregar_2(X,L,L) :-
   memberchk(X,L), !.
agregar_2(X,L,[X|L]).

% ------------------------------------------------------------------------------
% Ejercicio 3. Definir el predicado separa(L1,L2,L3) que separa la lista de
% números L1 en dos listas: L2 formada por los números positivos y L3 formada
% por los números negativos o cero. Por ejemplo,
%    ?- separa([2,0,-3,5,0,2],L2,L3).
%    L2 = [2, 5, 2].
%    L3 = [0, -3, 0]
%
% Escribir dos soluciones, una sin corte y otra con corte.
% ------------------------------------------------------------------------------

% 1ª solución (sin corte)
% =======================

separa_1([],[],[]).
separa_1([N|RL1],[N|RL2],L3) :-
   N > 0,
   separa_1(RL1,RL2,L3).
separa_1([N|RL1],L2,[N|RL3]) :-
   N =< 0,
   separa_1(RL1,L2,RL3).

% 2ª solución (con corte)
% =======================

separa_2([],[],[]).
separa_2([N|RL1],[N|RL2],L3) :-
   N > 0, !,
   separa_2(RL1,RL2,L3).
separa_2([N|RL1],L2,[N|RL3]) :-
   % N =< 0,
   separa_2(RL1,L2,RL3).

% ------------------------------------------------------------------------------
% Ejercicio 4: Definir el predicado suma_pares(L,N) que se verifica si N es
% la suma de todos los números pares de la lista L. Por ejemplo,
%    ?- suma_pares([2,3,4],N).
%    N = 6
%    ?- suma_pares([1,3,5,6,9,11,24],N).
%    N = 30
%
% Escribir una versión con negación y otra con corte.
% ------------------------------------------------------------------------------

% 1ª solución (con negación)
% ==========================

suma_pares_1([],0).
suma_pares_1([N|L],X) :-
   par(N),
   suma_pares_1(L,X1),
   X is X1 + N.
suma_pares_1([_N|L],X) :-
   not(par(_N)),
   suma_pares_1(L,X).

par(N):-
   N mod 2 =:= 0.

% 2ª solución (con corte)
% =======================

suma_pares_2([],0).
suma_pares_2([N|L],X) :-
   par(N), !,
   suma_pares_2(L,X1),
   X is X1 + N.
suma_pares_2([_N|L],X) :-
   % not(par(_N)),
   suma_pares_2(L,X).

% 3ª versión (con corte y acumulador)
% ===================================

suma_pares_3(L,X):-
   suma_pares_3_aux(L,0,X).

suma_pares_3_aux([],Ac,Ac).
suma_pares_3_aux([N|L],Ac,X) :-
   par(N), !,
   Ac1 is Ac + N,
   suma_pares_3_aux(L,Ac1,X).
suma_pares_3_aux([_N|L],Ac,X) :-
   % not(par(_N)),
   suma_pares_3_aux(L,Ac,X).

% Comparación de eficiencia
% =========================

% La comparación es
%    ?- time((X is 10^6, numlist(1,X,L),  suma_pares_1(L,N))).
%    % 6,500,006 inferences, 2.682 CPU in 2.688 seconds (100% CPU, 2423324 Lips)
%    X = 1000000,
%    L = [1, 2, 3, 4, 5, 6, 7, 8, 9|...],
%    N = 250000500000
%
%    ?- time((X is 10^6, numlist(1,X,L),  suma_pares_2(L,N))).
%    % 4,500,007 inferences, 2.111 CPU in 2.111 seconds (100% CPU, 2132088 Lips)
%    X = 1000000,
%    L = [1, 2, 3, 4, 5, 6, 7, 8, 9|...],
%    N = 250000500000.
%
%    ?- time((X is 10^6, numlist(1,X,L),  suma_pares_3(L,N))).
%    % 4,500,008 inferences, 0.417 CPU in 0.417 seconds (100% CPU, 10794334 Lips)
%    X = 1000000,
%    L = [1, 2, 3, 4, 5, 6, 7, 8, 9|...],
%    N = 250000500000.

% ------------------------------------------------------------------------------
% Ejercicio 6: Definir el predicado exponente_de_dos(N,Exp) que se verifica si
% E es el exponente de 2 en la descomposición de N como producto de factores
% primos. Por ejemplo,
%    ?- exponente_de_dos(40,E).
%    E = 3
%    ?- exponente_de_dos(49,E).
%    E = 0
%
% Escribir una versión con negación y otra con corte.
% ------------------------------------------------------------------------------

% 1ª solución (con negación)
% ==========================

exponente_de_dos_1(N,E):-
   N mod 2 =:= 0,
   N1 is N / 2,
   exponente_de_dos_1(N1,E1),
   E is E1 + 1.
exponente_de_dos_1(N,0) :-
   N mod 2 =\= 0.

% 2ª solución (con corte)
% =======================

exponente_de_dos_2(N,E):-
   N mod 2 =:= 0, !,
   N1 is N / 2,
   exponente_de_dos_2(N1,E1),
   E is E1 + 1.
exponente_de_dos_2(_,0).

% ------------------------------------------------------------------------------
% Ejercicio 4. En los distintos apartados de este ejercicio se usará la relación
% conc cuya definición se muestra a continuación.
% ------------------------------------------------------------------------------

conc([],L,L).
conc([X|L1],L2,[X|L3]) :- conc(L1,L2,L3).

% ------------------------------------------------------------------------------
% Ejercicio 1. Encontrar todas las listas L tal que al concatenar L con [c,d]
% nos devuelva [a,b,c,d].
% ------------------------------------------------------------------------------

% Solución
%    ?- conc(L,[c,d],[a,b,c,d]).
%    L = [a, b] ;
%    false.

% ------------------------------------------------------------------------------
% Ejercicio 4.2. Si obtener prefijos de esta forma fuera el único uso de conc en
% nuestro programa, ¿podríamos modificar su definición para mejorar la
% eficiencia?
% ------------------------------------------------------------------------------

conc_1([],L,L) :- !.
conc_1([X|RL1],L2,[X|RL3]) :- conc_1(RL1,L2,RL3).

% Ejemplo:
%    ?- conc_1(L,[c,d],[a,b,c,d]).
%    L = [a, b] ;
%    false.

% ------------------------------------------------------------------------------
% Ejercicio 4.3. ¿Serviría esa modificación para el uso general de conc?
% ------------------------------------------------------------------------------

% No, por ejemplo
%    ?- conc_1(L1,L2,[a,b,c]).
%    L1 = []
%    L2 = [a, b, c].
%
%    ?- conc(L1,L2,[a,b,c]).
%    L1 = [],
%    L2 = [a, b, c] ;
%    L1 = [a],
%    L2 = [b, c] ;
%    L1 = [a, b],
%    L2 = [c] ;
%    L1 = [a, b, c],
%    L2 = [] ;
%    false.

% ----------------------------------------------------------------------
% Ejercicio 5. Definir la relación crecimientos(+L1,-L2) que se
% verifique si L2 es la lista correspondientes a los crecimientos de la
% lista numérica L1; es decir, entre cada par de elementos consecutivos
% X e Y de L1 coloca el signo + si X < Y e y signo - en caso contrario.
% Por ejemplo,
%    ?- crecimientos([1,3,2,2,5,3],L).
%    L = [1, +, 3, -, 2, -, 2, +, 5, -]
% ----------------------------------------------------------------------

% 1ª solución (sin corte)
crecimientos_1([_],[]).
crecimientos_1([X,Y|L1],[X,+|L2]) :-
   X < Y,
   crecimientos_1([Y|L1],L2).
crecimientos_1([X,Y|L1],[X,-|L2]) :-
   X >= Y,
   crecimientos_1([Y|L1],L2).

% 1ª solución (sin corte)
crecimientos_2([_],[]).
crecimientos_2([X,Y|L1],[X,+|L2]) :-
   X < Y, !,
   crecimientos_2([Y|L1],L2).
crecimientos_2([X,Y|L1],[X,-|L2]) :-
   % X >= Y,
   crecimientos_2([Y|L1],L2).

