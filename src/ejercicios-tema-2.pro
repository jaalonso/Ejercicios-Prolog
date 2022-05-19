% ejercicios-tema-2.pl
% Tema 2: Listas, operadores y aritmética.
% José A. Alonso Jiménez <https://jaalonso.github.io>
% Sevilla, 18-mayo-2022
% ======================================================================

% ----------------------------------------------------------------------
% Ejercicio 1. Definir la  relación pertenece(X,L) que se verifica si X
% es un elemento de la lista L. Por ejemplo,
%    ?- pertenece(b,[a,b]).
%    true
%
%    ?- pertenece(c,[a,b]).
%    false.
%
%    ?- pertenece(X,[a,b]).
%    X = a ;
%    X = b ;
%    false.
%
% Nota: La relación predefinida correspondiente a pertenece es member.
% ----------------------------------------------------------------------

% 1ª solución
% ===========

pertenece(X,[X|_]).
pertenece(X,[_|L]) :-
   pertenece(X,L).

% 2ª solución
% ===========

pertenece_2(X,[Y|L]) :-
   X = Y ; pertenece_2(X,L).

% 3ª solución
% ===========

pertenece_3(X,L) :-
   append(_,[X|_],L).

% ----------------------------------------------------------------------
% Ejercicio 2. Definir la relación sublista(L1,L2) que se verifica si L1
% es una sublista de L2. Por ejemplo,
%    ?- sublista([b,c],[a,b,c,d]).
%    true ;
%    false.
%
%    ?- sublista([b,d],[a,b,c,d]).
%    false.
% ----------------------------------------------------------------------

sublista(L1,L2) :-
   append(_,L4,L2),
   append(L1,_,L4).

% ----------------------------------------------------------------------
% Ejercicio 3. Definir la relación último(L,X) que se verifica si el
% último elemento de la lista L es X. Por ejemplo,
%    ?- último([a,b,c],X).
%    X = c
%    ?- último(Xs,c).
%    Xs = [c] ;
%    Xs = [_1000, c]
%
% Nota: La relación predefinida correspondiente a último es last.
% ----------------------------------------------------------------------

% 1ª solución
% ===========

último([X],X).
último([_|L],X) :-
   último(L,X).

% 2ª solución
% ===========

último_2([X|Xs],U) :-
   último_2_aux(Xs,X,U).

último_2_aux([], U, U).
último_2_aux([X|Xs], _, U) :-
    último_2_aux(Xs, X, U).

% 3ª solución
% ===========

último_3(L,X) :-
   append(_,[X],L).

% ----------------------------------------------------------------------
% Ejercicio 4. Definir la relación inversa(L1,L2) que se verifica si L2
% es la lista L1 en orden inverso, Por ejemplo,
%    ?- inversa([a,b,c],L).
%    L = [c, b, a].
%
%    ?- inversa([a,[b,c],d,e],L).
%    L = [e, d, [b, c], a].
%
% Nota: La relación predefinida correspondiente a inversa es reverse.
% ----------------------------------------------------------------------

inversa([],[]).
inversa([X|L1],L2) :-
   inversa(L1,L3),
   append(L3,[X],L2).

% ----------------------------------------------------------------------
% Ejercicio 5. Definir la relación palíndromo(L) que se verifica si L es
% un palíndromo; es decir, da igual leerlo de izquierda a derecha que
% leerlo de derecha a izquierda. Por ejemplo,
%    ?- palíndromo([r,o,m,a,y,a,m,o,r]).
%    true.
% ----------------------------------------------------------------------

palíndromo(L) :-
   inversa(L,L).

% ----------------------------------------------------------------------
% Ejercicio 6. Definir la relación selecciona(X,L1,L2) que se verifica
% si X es un elemento de la lista L1 y L2 es la lista de los restantes
% elementos. Por ejemplo,
%    ?- selecciona(X,[a,b,c],L).
%    X = a,
%    L = [b, c] ;
%    X = b,
%    L = [a, c] ;
%    X = c,
%    L = [a, b] ;
%    false.
%
%    ?- selecciona(a,L1,[b,c]).
%    L1 = [a, b, c] ;
%    L1 = [b, a, c] ;
%    L1 = [b, c, a] ;
%    false.
%
%    ?- selecciona(a,L1,L2).
%    L1 = [a|L2] ;
%    L1 = [_8510, a|_8518],
%    L2 = [_8510|_8518]
%
% Nota: La relación predefinida correspondiente a selecciona es select.
% ----------------------------------------------------------------------

selecciona(X,[X|L],L).
selecciona(X,[Y|L1],[Y|L2]) :-
   selecciona(X,L1,L2).

% ----------------------------------------------------------------------
% Ejercicio 7. Definir la relación máximo(X,Y,Z) que se verifica si Z es
% el máximo de los números X e Y. Por ejemplo,
%    ?- máximo(3,5,Z).
%    Z = 5.
%
%    ?- máximo(2,3,X).
%    X = 3.
%
%    ?- máximo(3,2,X).
%    X = 3 ;
%    false.
% ----------------------------------------------------------------------

% 1ª solucion
máximo(X,Y,X) :-
   X >= Y.
máximo(X,Y,Y) :-
   X < Y.

% 2ª solución
máximo_2(X,Y,Z) :-
   Z is max(X,Y).

% Nota: El comportamiento es diferente si no están instanciados los dos
% primeros argumentos. Por ejemplo.
%    ?- máximo(3,5,Z).
%    Z = 5.
%
%    ?- máximo(3,Y,5).
%    Y = 5.
%
%    ?- máximo_2(3,5,Z).
%    Z = 5.
%
%    ?- máximo_2(3,Y,5).
%    ERROR: Arguments are not sufficiently instantiated

% ----------------------------------------------------------------------
% Ejercicio 8 (Algoritmo de Euclides). Dados dos enteros positivos X e
% Y, el máximo común divisor (mcd) D puede obtenerse de la siguiente
% manera:
% + Si X e Y son iguales, entonces D es igual a X
% + Si X < Y, entonces D es igual al máximo común divisor de X y la
%   diferencia Y-X.
% + Si Y < X, entonces D es igual al máximo común divisor de X y la
%   diferencia X-Y.
%
% Definir el predicado mcd(X,Y,D) que se verifica si D es el máximo
% común divisor de los enteros positivos X e Y. Por ejemplo.
%    ?- mcd(5,5,X).
%    X = 5
%    ?- mcd(6,10,X).
%    X = 2
%    ?- mcd(10,6,X).
%    X = 2
% ----------------------------------------------------------------------

% 1ª solución
% ===========

mcd(X,X,X).
mcd(X,Y,Z) :-
   X < Y,
   Y1 is Y - X,
   mcd(X,Y1,Z).
mcd(X,Y,Z) :-
   X > Y,
   mcd(Y,X,Z).

% 2ª solución
% ===========

mcd_2(X,Y,Z) :-
   Z is gcd(X, Y).

% ----------------------------------------------------------------------
% Ejercicio 9. Definir la relación longitud(L,N) que se verifica si N es
% la longitud de la lista L. Por ejemplo,
%    ?- longitud([],N).
%    N = 0.
%
%    ?- longitud([a,b,c],N).
%    N = 3.
%
%    ?- longitud([a,[b,c]],N).
%    N = 2.
%
% Nota: La relación predefinida correspondiente a longitud es length.
% ----------------------------------------------------------------------

longitud([],0).
longitud([_X|L],N) :-
   longitud(L,M),
   N is M + 1.

% ----------------------------------------------------------------------
% Ejercicio 10. Definir la relación máximo_lista(L,N) que se verifica si
% N es el máximo de los elementos de la lista de números L. Por ejemplo,
%    ?- máximo_lista([1,3,9,5],N).
%    N = 9
%
% Nota: La relación predefinida correspondiente a máximo_lista es
% max_list.
% ----------------------------------------------------------------------

% 1ª solución
máximo_lista([X],X).
máximo_lista([X,Y|L],N) :-
   máximo(X,Y,Z),
   máximo_lista([Z|L],N).

% 2ª solución
máximo_lista_2([X],X).
máximo_lista_2([X,Y|L],N) :-
   Z is max(X,Y),
   máximo_lista_2([Z|L],N).

% ----------------------------------------------------------------------
% Ejercicio 11. Definir la relación entre(N1,N2,X) que se verifica si X
% es mayor o igual que N1 y menor o igual que N2. Por ejemplo,
%    ?- entre(2,5,X).
%    X = 2 ;
%    X = 3 ;
%    X = 4 ;
%    X = 5 ;
%    false.
%
%    ?- entre(2,1,X).
%    false.
%
% Nota: La relación predefinida correspondiente a entre es between.
% ----------------------------------------------------------------------

entre(N1,N2,N1) :-
   N1 =< N2.
entre(N1,N2,X) :-
   N1 < N2,
   N3 is N1 + 1,
   entre(N3,N2,X).

% ----------------------------------------------------------------------
% Ejercicio 12. Se considera la siguiente base de conocimiento
%    :- op(800,xfx,es).
%    :- op(400,yfx,de).
%    el_libro de ciencias de juan es rojo.
% ¿Qué responde Prolog a las siguientes preguntas?
%    ?- X es rojo.
%    ?- X de Y es rojo.
%    ?- el_libro de X es rojo.
% ----------------------------------------------------------------------

% Solución:
%    ?- X es rojo.
%    X = el_libro de ciencias de juan
%    true.
%
%    ?- X de Y es rojo.
%    X = el_libro de ciencias
%    Y = juan
%    true.
%
%    ?- el_libro de X es rojo.
%    false.

% ----------------------------------------------------------------------
% Ejercicio 13. Se considera la siguiente base de conocimiento
%    :- op(500,yfx,a).
%    b a c a l a o.
% 1. ¿Qué responde Prolog a las siguientes preguntas?
%       ?- M a l a S.
%       ?- b a c a S.
% 2. ¿Y si cambiamos la directiva por :- op(500,xfy,a).?
% ----------------------------------------------------------------------

% Solución del apartado 1
% =======================

% La directiva
%    :- op(500,yfx,a)
% indica que a es un operador infijo que asocia por la izquierda. Por
% tanto, el término correspondiente al hecho
%    b a c a l a o
% es
%    a(a(a(b,c),l),o).
%
% El término correspondiente a
%    M a l a S
% es
%    a(a(M,l),S)
% y la respuesta de Prolog es
%    ?- M a l a S.
%    M = b a c
%    S = o
%
% El término correspondiente
%    a b a c a S
% es
%    a(a(b,c),S)
% y la respuesta de Prolog es
%    ?- b a c a S.
%    false.

% Solución del apartado 2
% =======================

% La directiva
%    :- op(500,xfy,a)
% indica que a es un operador infijo que asocia por la derecha. Por
% tanto, el término correspondiente al hecho
%    b a c a l a o
% es
%   a(b,a(c,a(l,o))).
%
% El término correspondiente a
%    M a l a S
% es
%    a(M,a(l,S))
% y la respuesta de Prolog es
%    ?- M a l a S.
%    false.
%
% El término correspondiente
%    a b a c a S
% es
%    a(b,a(c,S))
% y la respuesta de Prolog es
%    ?- b a c a S.
%    S = l a o
