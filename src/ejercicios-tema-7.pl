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
