% ejercicios-tema-7.pl
% Ejercicios del tema 7 (Aplicaciones: Grafos, reinas y mapas).
% José A. Alonso Jiménez <https://jaalonso.github.io>
% Sevilla, 24-mayo-2022
% ======================================================================

% Nota. Usaremos las relaciones de camino.pl
:- [camino].

% También usaremos los siguientes ejemplos de grafo
grafo(g1,[a-b,b-c,b-d,c-d]).
grafo(g2,[a-b,c-d]).
grafo(g3,[a-b,b-c,b-d]).
grafo(g4,[a-b,b-c]).

% ----------------------------------------------------------------------
% Ejercicio 1. Definir la relación conectado(+G) que se verifica si el
% grafo G está conectado; es decir, existe un camino entre cada par de
% vértices distintos. Por ejemplo,
%    ?- conectado(g1).
%    true.
%
%    ?- conectado(g2).
%    false.
%
%    ?- conectado(g3).
%    true.
% ----------------------------------------------------------------------

conectado(G) :-
   not((nodo(G,X), nodo(G,Y), not(camino(G,X,Y,_)))).

% ----------------------------------------------------------------------
% Ejercicio 2. Definir la relación tiene_ciclos(+G) que se verifica si
% en el grafo G hay ciclos. Por ejemplo,
%    ?- tiene_ciclos(g1).
%    true
%
%    ?- tiene_ciclos(g2).
%    false.
%
%    ?- tiene_ciclos(g3).
%    false.
% ----------------------------------------------------------------------

tiene_ciclos(G) :-
   adyacente(G,X,Y),
   camino(G,X,Y,[X,_,_|_]).

% ----------------------------------------------------------------------
% Ejercicio 3. Definir la relación es_árbol(+G) que se verifica si G es
% un árbol; es decir, G es un grafo conectado sin ciclos. Por ejemplo,
%    ?- es_árbol(g1).
%    false.
%
%    ?- es_árbol(g2).
%    false.
%
%    ?- es_árbol(g3).
%    true.
% ----------------------------------------------------------------------

es_árbol(G) :-
   conectado(G),
   not(tiene_ciclos(G)).

% ----------------------------------------------------------------------
% Ejercicio 4. Definir la relación recubre(+G1,+G2) que se verifica si
% el grafo G1 recubre el grafo G2 (es decir, todos los nodos del grafo
% G2 son nodos del grafo G1). Por ejemplo,
%    ?- recubre(g3,g4).
%    true.
%
%    ?- recubre(g4,g3).
%    false.
% ----------------------------------------------------------------------

recubre(G1,G2) :-
   not((nodo(G2,X), not(nodo(G1,X)))).

% ----------------------------------------------------------------------
% Ejercicio 4. Definir la relación árbol_de_expansión(+G,?A) que se
% verifica si A es un árbol de expansión de G; es decir, A es un
% subgrafo de G que es un árbol y recubre a G. Por ejemplo,
%    ?- árbol_de_expansión(g1,A).
%    A = grafo(ae2, [a-b, b-c, b-d]) ;
%    A = grafo(ae3, [a-b, b-c, c-d]) ;
%    A = grafo(ae5, [a-b, b-d, c-d]) ;
%    false.
% ----------------------------------------------------------------------

:- dynamic grafo/2.

árbol_de_expansión(G,A) :-
   reset_gensym(ae),
   árbol_de_expansión_aux(G,A).

árbol_de_expansión_aux(G,A) :-
   arcos(G,L1),
   subconjunto(L2,L1),
   gensym(ae,E),
   A =.. [grafo,E,L2],
   asserta(A),
   es_árbol(E),
   recubre(E,G).

% subconjunto(?L1,+L2) se verifica si L1 es un subconjunto de L2. Por
% ejemplo,
%    ?- subconjunto(L,[a,b,c]).
%    L = [a, b, c] ;
%    L = [a, b] ;
%    L = [a, c] ;
%    L = [a] ;
%    L = [b, c] ;
%    L = [b] ;
%    L = [c] ;
%    L = [].
subconjunto([],[]).
subconjunto([X|L1],[X|L2]) :-
   subconjunto(L1,L2).
subconjunto(L1,[_|L2]) :-
   subconjunto(L1,L2).

% ----------------------------------------------------------------------
% Ejercicio 5.1. Mediante la relación lista_de_clase(C,A,L) se
% representa la información de los alumnos según los cursos y
% asignaturas, de forma que C es el curso, A es la asignatura y L es la
% lista de los alumnos de dicha asignatura. A lo largo del ejercicio
% vamos a usar como ejemplo la siguiente información.
%    lista_de_clase(c1,asig1,[a1,a5]).
%    lista_de_clase(c1,asig2,[a1,a2,a3]).
%    lista_de_clase(c1,asig3,[a1,a3]).
%    lista_de_clase(c1,asig4,[a2,a4,a6,a8]).
%    lista_de_clase(c1,asig5,[a2,a4,a5]).
%    lista_de_clase(c1,asig6,[a6,a7]).
%    lista_de_clase(c1,asig7,[a3,a7]).
%    lista_de_clase(c1,asig8,[a6,a7,a8]).
%    lista_de_clase(c2,asig9,[a9,a11]).
%    lista_de_clase(c2,asig10,[a10,a12]).
%    lista_de_clase(c2,asig11,[a10,a11]).
%    lista_de_clase(c2,asig12,[a9,a12]).
%
% Definir la relación asignaturas(+C,-L) que se verifique si L es la
% lista de asignaturas del curso C. Por ejemplo,
%    ?- asignaturas(c1,L).
%    L = [asig1,asig2,asig3,asig4,asig5,asig6,asig7,asig8].
%    
%    ?- asignaturas(c2,L).
%    L = [asig9,asig10,asig11,asig12].
% ----------------------------------------------------------------------

lista_de_clase(c1,asig1,[a1,a5]).
lista_de_clase(c1,asig2,[a1,a2,a3]).
lista_de_clase(c1,asig3,[a1,a3]).
lista_de_clase(c1,asig4,[a2,a4,a6,a8]).
lista_de_clase(c1,asig5,[a2,a4,a5]).
lista_de_clase(c1,asig6,[a6,a7]).
lista_de_clase(c1,asig7,[a3,a7]).
lista_de_clase(c1,asig8,[a6,a7,a8]).
lista_de_clase(c2,asig9,[a9,a11]).
lista_de_clase(c2,asig10,[a10,a12]).
lista_de_clase(c2,asig11,[a10,a11]).
lista_de_clase(c2,asig12,[a9,a12]).

asignaturas(C,L):-
   findall(X,lista_de_clase(C,X,_),L).

% ----------------------------------------------------------------------
% Ejercicio 5.2. Definir la relación grupo_incompatible(+L) que se
% verifique si la lista de asignaturas L es incompatible (es decir,
% algún alumno está en las listas de clase de más de una asignatura de
% la lista L). Por ejemplo,
%    ?- grupo_incompatible([asig1,asig2]).
%    true 
%    ?- grupo_incompatible([asig1,asig4,asig7]).
%    false.
% ----------------------------------------------------------------------

grupo_incompatible(L):-
   select(X,L,R),
   member(Y,R),
   lista_de_clase(_,X,LX),
   lista_de_clase(_,Y,LY),
   member(A,LX),
   member(A,LY).

% ----------------------------------------------------------------------
% Ejercicio 5.3. Definir la relación lista_incompatible(+L) que
% verifique si la lista de grupos de asignaturas L es incompatible (es
% decir, contiene algún grupo incompatible de asignaturas). Por ejemplo,
%    ?- lista_incompatible([[asig9, asig12], [asig11, asig10]]).
%    true 
%    ?- lista_incompatible([[asig11, asig12], [asig9, asig10]]).
%    false.
% ----------------------------------------------------------------------

lista_incompatible(P) :-
   member(G,P),
   grupo_incompatible(G).

% ----------------------------------------------------------------------
% Ejercicio 5.4. Definir la relación extensión(+L1,+X,-L2) que se
% verifique si L2 es la lista obtenida añadiendo X como primer elemento
% de un elemento de L1 o L2 es la lista obtenida añadiendo [X] a L1. Por
% ejemplo,
%    ?- extensión([[a], [b, c]],d,E).
%    E = [[d,a],[b,c]] ;
%    E = [[a],[d,b,c]] ;
%    E = [[a],[b,c],[d]].
% ----------------------------------------------------------------------

extensión([],X,[[X]]).
extensión([L|L1],X,[[X|L]|L1]).
extensión([L|L1],X,[L|L2]) :-
   extensión(L1,X,L2).

% ----------------------------------------------------------------------
% Ejercicio 5.5. Definir la relación partición(+L,-P) que se verifique
% si P es una partición de la lista L (es decir, un conjunto obtenido
% distribuyendo los elementos de L en conjuntos no vacíos y sin
% elementos comunes). Por ejemplo,
%    ?- partición([a,b,c],P).
%    P = [[a,b,c]] ;
%    P = [[b,c],[a]] ;
%    P = [[a,c],[b]] ;
%    P = [[c],[a,b]] ;
%    P = [[c],[b],[a]].
% ----------------------------------------------------------------------

partición([],[]).
partición([X|L1],L2) :-
   partición(L1,L3),
   extensión(L3,X,L2).
 
% ----------------------------------------------------------------------
% Ejercicio 5.6. Definir la relación agrupación_compatible_de_asignaturas(+C,-P)
% que se verifique si P es una partición compatible de las asignaturas
% del curso C. Por ejemplo,
%    ?- agrupación_compatible_de_asignaturas(c2,P).
%    P = [[asig11,asig12],[asig9,asig10]] ;
%    P = [[asig11,asig12],[asig10],[asig9]] ;
%    P = [[asig12],[asig11],[asig9,asig10]] 
% ----------------------------------------------------------------------

agrupación_compatible_de_asignaturas(C,P) :-
   asignaturas(C,L),
   partición(L,P),
   not(lista_incompatible(P)).
   
% ----------------------------------------------------------------------
% Ejercicio 5.7. Definir la relación agrupación_compatible_minimal(+C,-P)
% que se verifique si P es una partición compatible de las asignaturas
% del curso C con el menor número posible de grupos de asignaturas. Por
% ejemplo,
%    ?- agrupación_compatible_minimal(c2,P).
%    P = [[asig11,asig12],[asig9,asig10]] ;
%    false.
% ----------------------------------------------------------------------

agrupación_compatible_minimal(C,P) :-
   agrupación_compatible_de_asignaturas(C,P),
   length(P,N),
   not((agrupación_compatible_de_asignaturas(C,P1),length(P1,M),M < N)).

% ----------------------------------------------------------------------
% Ejercicio 5.8. Calcular las agrupaciones compatibles minimales del
% curso c1.
% ----------------------------------------------------------------------

% Solución
%    ?- agrupación_compatible_minimal(c1,P).
%    P = [[asig3,asig5,asig8],[asig1,asig4,asig7],[asig2,asig6]] ;
%    P = [[asig2,asig8],[asig1,asig4,asig7],[asig3,asig5,asig6]] ;
%    false.
