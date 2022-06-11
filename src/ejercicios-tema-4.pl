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
suma_pares_1([N|L],X) :-
   not(par(N)),
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
% Ejercicio 5. Definir el predicado exponente_de_dos(N,Exp) que se verifica si
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
% Ejercicio 6. En los distintos apartados de este ejercicio se usará la relación
% conc cuya definición se muestra a continuación.
% ------------------------------------------------------------------------------

conc([],L,L).
conc([X|L1],L2,[X|L3]) :- conc(L1,L2,L3).

% ------------------------------------------------------------------------------
% Ejercicio 6.1. Encontrar todas las listas L tal que al concatenar L con [c,d]
% nos devuelva [a,b,c,d].
% ------------------------------------------------------------------------------

% Solución
%    ?- conc(L,[c,d],[a,b,c,d]).
%    L = [a, b] ;
%    false.

% ------------------------------------------------------------------------------
% Ejercicio 6.2. Si obtener prefijos de esta forma fuera el único uso de conc en
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
% Ejercicio 6.3. ¿Serviría esa modificación para el uso general de conc?
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
% Ejercicio 7. Definir la relación crecimientos(+L1,-L2) que se
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

% 2ª solución (con corte)
crecimientos_2([_],[]).
crecimientos_2([X,Y|L1],[X,+|L2]) :-
   X < Y, !,
   crecimientos_2([Y|L1],L2).
crecimientos_2([X,Y|L1],[X,-|L2]) :-
   % X >= Y,
   crecimientos_2([Y|L1],L2).

% ----------------------------------------------------------------------
% Ejercicio 8.1. Definir la relación menor_divisor_propio(+N,?X) que
% se verifique si X es el menor divisor de N mayor o igual que 2. Por
% ejemplo,
%    ?- menor_divisor_propio(30,X).
%    X = 2.
%    ?- menor_divisor_propio(3,X).
%    X = 3.
% ----------------------------------------------------------------------

menor_divisor_propio(N,X) :-
   N1 is floor(sqrt(N)),
   between(2,N1,X),
   N mod X =:= 0, !.
menor_divisor_propio(N,N).

% -----------------------------------------------------------------------
% Ejercicio 8.2. Definir la relación factorización(+N,-L) que se
% verifique si L es la lista correspondiente a la descomposición del
% número N en factores primos (se considera los que elementos de L
% están ordenados de manera creciente). Por ejemplo,
%    ?- factorización(12,L).
%    L = [2, 2, 3] ;
%    false.
%    ?- factorización(1,L).
%    L = [] ;
%    false.
% ----------------------------------------------------------------------

factorización(1,[]).
factorización(N,[X|L]) :-
   N > 1,
   menor_divisor_propio(N,X),
   N1 is N/X,
   factorización(N1,L).

% ----------------------------------------------------------------------   
% Ejercicio 9. Definir la relación calcula(+N,+M,?X) que se verifique
% si X es el menor múltiplo de N tal que la suma de sus dígitos es
% mayor que M. Por ejemplo,
%    ?- calcula(3,10,X).
%    X = 39.
%    ?- calcula(7,20,X).
%    X = 399.
% ----------------------------------------------------------------------

calcula(N,M,X) :-
   múltiplo(N,X),
   suma_dígitos(X,N1),
   N1 > M, !.

% múltiplo(+N,-X) se verifica si X es un múltiplo de N. Por ejemplo,
%    ?- múltiplo(5,X).
%    X = 5 ;
%    X = 10 ;
%    X = 15
múltiplo(N,N).
múltiplo(N,M) :-
   múltiplo(N,N1),
   M is N+N1.

% La relación suma_dígitos(+N,-S) se verifica si S es la suma de los
% dígitos del número N. Por ejemplo,
%    ?- suma_dígitos(237,S).
%    S = 12
suma_dígitos(N,N) :-
   N < 10, !.
suma_dígitos(N,S) :-
   % N >= 10,
   N1 is N // 10,
   R is N - 10*N1,
   suma_dígitos(N1,S1),
   S is S1 + R.

% 2ª definición de suma_digitos
suma_dígitos_2(N,S) :-
   dígitos(N,L),
   sum_list(L,S).

% dígitos(N,L) se verifica si L es la lista de los dígitos de N. Por
% ejemplo.
%    ?- dígitos(325,L).
%    L = [3,2,5].
dígitos(N,L) :-
   name(N,L1),
   maplist(plus(-48),L1,L).

% ----------------------------------------------------------------------   
% Ejercicio 10. Un número es libre de cuadrados si no es divisible por
% el cuadrado de ningún número mayor que 1.
% 
% Definir la relación libre_de_cuadrados(+N) que se verifique si el
% número N es libre de cuadrados. Por ejemplo,
%    ?- libre_de_cuadrados(30).
%    true.
%    ?- libre_de_cuadrados(12).
%    false.
% ----------------------------------------------------------------------

% 1ª solución
libre_de_cuadrados(N) :-
   M is floor(sqrt(N)),
   not((between(2,M,X), N mod (X*X) =:= 0)).

% 2ª solución
libre_de_cuadrados_2(N) :-
   M is floor(sqrt(N)),
   forall(between(2,M,X), N mod (X*X) =\= 0).

% ----------------------------------------------------------------------
% Ejercicio 11. Definir la relación suma_libres_de_cuadrados(+L,-S) que
% se verifique si S es la suma de los números libres de cuadrados la
% lista numérica L. Por ejemplo,
%    ?- suma_libres_de_cuadrados([6,12,18,30],S).
%    S = 36.
% ----------------------------------------------------------------------

% 1ª solución (sin cortes)
suma_libres_de_cuadrados_1([],0).
suma_libres_de_cuadrados_1([X|L],S) :-
   libre_de_cuadrados(X),
   suma_libres_de_cuadrados_1(L,S1),
   S is X+S1.
suma_libres_de_cuadrados_1([X|L],S) :-
   not(libre_de_cuadrados(X)),
   suma_libres_de_cuadrados_1(L,S).

% 2ª solución (con cortes)
suma_libres_de_cuadrados_2([],0).
suma_libres_de_cuadrados_2([X|L],S) :-
   libre_de_cuadrados(X), !,
   suma_libres_de_cuadrados_2(L,S1),
   S is X+S1.
suma_libres_de_cuadrados_2([_X|L],S) :-
   % not(libre_de_cuadrados(_X)),
   suma_libres_de_cuadrados_2(L,S).

% 3ª solución
suma_libres_de_cuadrados_3(L,S) :-
   include(libre_de_cuadrados, L, L1),
   sum_list(L1,S).

% ----------------------------------------------------------------------
% Ejercicio 12. Definir la relación longitud_scm(+L1,+L2,-N) que se
% verifique si N es la longitud de las subsucesiones comunes maximales
% de las listas L1 y L2. Por ejemplo,
%    ?- longitud_scm([2,1,4,5,2,3,5,2,4,3],[1,7,5,3,2],N).
%    N = 4 ;
%    false.
% ya que [1,5,3,2] es una subsucesión de las dos listas y no poseen
% ninguna otra subsucesión común de mayor longitud. Obsérvese que los
% elementos de la subsucesión no son necesariamente elementos adyacentes
% en las listas.
% ----------------------------------------------------------------------

longitud_scm([],_,0).
longitud_scm(_,[],0).
longitud_scm([X|L1],[X|L2],N) :-
   !, longitud_scm(L1,L2,M),
   N is M+1.
longitud_scm([X|L1],[Y|L2],N) :-
   % X \= Y,
   longitud_scm(L1,[Y|L2],N1),
   longitud_scm([X|L1],L2,N2),
   N is max(N1,N2).

% ----------------------------------------------------------------------
% Ejercicio 13.1. Definir la relación repetido(-A,+L) que se verifique
% si el elemento A está repetido (i.e. ocurre más de una vez) en la
% lista L. Por ejemplo,
%    ?- repetido(A,[1,2,1,3,4,3]).
%    A = 1 ;
%    A = 1 ;
%    A = 3 ;
%    A = 3.
%    ?- repetido(A,[1,2,5]).
%    false.
% ----------------------------------------------------------------------

repetido(A,L) :-
   select(A,L,R),
   memberchk(A,R).

% ----------------------------------------------------------------------
% Ejercicio 13.2. Definir la relación elimina(+X,+L1,-L2) que se
% verifique si L2 es la lista obtenida eliminando todas las ocurrencias
% de X en la lista L1. Por ejemplo,
%    ?- elimina(a,[1,a,b,3,a,a,4,a,c],L).
%    L = [1, b, 3, 4, c]
% ----------------------------------------------------------------------

elimina(_,[],[]).
elimina(X,[X|L1],L2) :-
   elimina(X,L1,L2).
elimina(X,[Y|L1],[Y|L2]) :-
   X \= Y,
   elimina(X,L1,L2).

% ----------------------------------------------------------------------
% Ejercicio 13.3. Definir la relación repetidos(+L1,-L2) que se
% verifique si L2 es la lista de los elementos repetidos de la lista
% L1. Por ejemplo, 
%   ?- repetidos([1,2,4,3,4,1,3,5],L).
%   L = [1, 4, 3] 
% ----------------------------------------------------------------------

% 1ª solución
repetidos_1([],[]).
repetidos_1([X|L1],[X|L2]) :-
   memberchk(X,L1), 
   elimina(X,L1,L3),
   repetidos_1(L3,L2).
repetidos_1([X|L1],L2) :-
   not(memberchk(X,L1)),
   repetidos_1(L1,L2).

% 2ª solución
repetidos_2([],[]).
repetidos_2([X|L1],[X|L2]) :-
   memberchk(X,L1), !,
   elimina(X,L1,L3),
   repetidos_2(L3,L2).
repetidos_2([_X|L1],L2) :-
   % not(memberchk(_X,L1)),
   repetidos_2(L1,L2).

% --------------------------------------------------------------------
% Ejercicio 14. Definir la relación subconjunto_maximal(+L1,-L2) que
% se verifica si L2 es un subconjunto maximal de L1 (es decir, es un
% conjunto de elementos de L1 tal que sólo existe un elemento de L1 que
% no pertenece a L2). Por ejemplo,
%    ?- subconjunto_maximal([c,b,a,b,c,a,c],L).
%    L = [b,a] ;
%    L = [c,a] ;
%    L = [c,b].
% --------------------------------------------------------------------
  
subconjunto_maximal(L1,L2) :-
   list_to_set(L1,L3),
   select(_,L3,L2).

% ----------------------------------------------------------------------
% Ejercicio 15. % Definir la relación suma_posiciones(+N,+L,-S) que se
% verifique si S es la suma de los elementos de la lista que ocupan las
% posiciones que son múltiplos de N. Por ejemplo,
%   ?- suma_posiciones(2,[3,5,7,9,1,2],S).
%   S = 16.
%   ?- suma_posiciones(3,[3,5,7,9,1,2],S).
%   S = 9.
% ----------------------------------------------------------------------

suma_posiciones(N,L,S) :-
   elemento_y_resto(N,L,X,L1), !,
   suma_posiciones(N,L1,S1),
   S is X+S1.
suma_posiciones(_,_,0).

% elemento_y_resto(+N,+L1,-X,-L2) se verifica si X es el elemento
% N-ésimo de L1 y L2 es la lista L1 a partir del elemento X. Por
% ejemplo,
%   ?- elemento_y_resto(3,[3,5,7,9,1,2],X,L).
%   X = 7
%   L = [9, 1, 2]. 
elemento_y_resto(N,L1,X,L2) :-
   length(L,N),
   append(L,L2,L1),
   last(L,X).

% ----------------------------------------------------------------------
% Ejercicio 16. Definir la relación comprimida(+L1,-L2) que se verifique
% si L2 es la lista obtenida sustituyendo cada sucesión de un elemento
% de L1 por dicho elemento. Por ejemplo,
%    ?- comprimida([a,b,b,a,a,a,c,c,b,b,b],L).
%    L = [a, b, a, c, b] 
% ----------------------------------------------------------------------

% 1ª solución (sin corte)
comprimida([],[]).
comprimida([X],[X]).
comprimida([X,X|L1],L2) :-
   comprimida([X|L1],L2).
comprimida([X,Y|L1],[X|L2]) :-
   X \= Y,
   comprimida([Y|L1],L2).

% 2ª solución (con corte)
comprimida_2([],[]).
comprimida_2([X],[X]).
comprimida_2([X,Y|L1],L2) :-
   X = Y, !,
   comprimida_2([X|L1],L2).
comprimida_2([X,Y|L1],[X|L2]) :-
   % X \= Y,
   comprimida_2([Y|L1],L2).

% ----------------------------------------------------------------------   
% Ejercicio 17. Definir la relación empaquetada(+L1,-L2) que se
% verifique si L2 es la lista obtenida sustituyendo cada sucesión de un
% elemento de L1 por la lista formada por dicha sucesión. Por ejemplo,
%    ?- empaquetada([a,b,b,a,a,a,c,c,b,b,b],L).
%    L = [[a], [b, b], [a, a, a], [c, c], [b, b, b]] 
% ----------------------------------------------------------------------

empaquetada([],[]).
empaquetada([X|L1],[L2|L3]) :-
   empaquetada_aux(X,L1,L4,L2),
   empaquetada(L4,L3).

% empaquetada_aux(X,L1,L4,L2) se verifica si L4 es la lista obtenida
% eliminando en L1 todas las ocurrencias iniciales de X y L2 es la
% lista formada por X y las ocurrencias iniciales de X en L1; por
% ejemplo,
%    ?- empaquetada_aux(a,[a,a,c,c,b,b,b],L4,L2).
%    L4 = [c, c, b, b, b]
%    L2 = [a, a, a] 
empaquetada_aux(X,[],[],[X]).
empaquetada_aux(X,[X|L1],L4,[X|L2]) :-
   empaquetada_aux(X,L1,L4,L2).
empaquetada_aux(X,[Y|L1],[Y|L1],[X]) :-
   X \= Y.

% La definición anterior puede transformarse introduciendo corte en
empaquetada_aux_2(X,[],[],[X]).
empaquetada_aux_2(X,[X|L1],L4,[X|L2]) :-
   !,
   empaquetada_aux_2(X,L1,L4,L2).
empaquetada_aux_2(X,[Y|L1],[Y|L1],[X]).

% ----------------------------------------------------------------------
% Ejercicio 18. Definir la relación codificada(+L1,-L2) que se verifique
% si L2 es la codificación por longitud de la lista L1; es decir, las
% sucesiones de un mismo elemento X de L1 se codifican por términos de
% la forma N-X donde N es la longitud de la sucesión. Por ejemplo,
%    ?- codificada([a,b,b,a,a,a,c,c,b,b,b],L).
%    L = [1-a, 2-b, 3-a, 2-c, 3-b] 
% ----------------------------------------------------------------------

% 1ª solución
% ===========

codificada(L1,L2) :-
   empaquetada(L1,L),
   codificada_aux(L,L2).

% codificada_aux(+L1,-L2) se verifica si, suponiendo que L1 es una lista
% de la forma [[E1,...,E1],...,[Em,....Em]]), L2 es la lista
% [N1-E1,...Nm-Em] donde Ni es la longitud de [Ei,...,Ei]. Por ejemplo.
%    ?- codificada_aux([[a],[b,b],[a,a,a],[c,c],[b,b,b]],L).
%    L = [1-a, 2-b, 3-a, 2-c, 3-b].
codificada_aux([],[]).
codificada_aux([[X|Y]|L1],[N-X|L2]) :-
   length([X|Y],N),
   codificada_aux(L1,L2).

% 2ª solución
% ===========

codificada_2(L1,L2) :-
   empaquetada(L1,L),
   codificada_aux_2(L,L2).

codificada_aux_2(L1,L2) :-
   bagof(N-X, X^L3^(member([X|L3],L1), length([X|L3],N)), L2).

