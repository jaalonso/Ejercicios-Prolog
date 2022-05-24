% camino.pl
% Camino en grafos.
% José A. Alonso Jiménez <https://jaalonso.github.io>
% Sevilla, 22-mayo-2022
% =============================================================================

% Se usa la representacion de grafos
:- [representacion_de_grafos].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% § Caminos                                                                 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% camino(+G,+A,+Z,-C) se verifica si C es un camino en el grafo G desde el nodo
% A al Z. Por ejemplo,
%    ?- camino(ejemplo1,a,d,C).
%    C = [a, b, d] ;
%    C = [a, b, c, d] ;
%    false.
%    ?- camino(andalucía,sevilla,granada,C).
%    C = [sevilla, huelva, cádiz, málaga, córdoba, jaén, almería, granada] ;
%    C = [sevilla, cádiz, málaga, córdoba, jaén, almería, granada]
%
%    ?- findall(C,camino(andalucía,sevilla,granada,C),_L), length(_L,N).
%    N = 16.
camino(G,A,Z,C) :-
   camino_aux(G,A,[Z],C).

% camino_aux(+G,+A,+CP,-C) se verifica si C es una camino en G compuesto de un
% camino desde A hasta el primer elemento del camino parcial CP (con nodos
% distintos a los de CP) junto CP.
camino_aux(_,A,[A|C1],[A|C1]).
camino_aux(G,A,[Y|C1],C) :-
   adyacente(G,X,Y),
   not(member(X,[Y|C1])),
   camino_aux(G,A,[X,Y|C1],C).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% § Caminos hamiltonianos                                                   %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% hamiltoniano(+G,-H) se verifica si H es un camino hamiltoniano en el grafo G
% (es decir, es un camino en G que pasa por todos sus nodos). Por ejemplo,
%    ?- hamiltoniano(ejemplo1,H).
%    H = [a, b, c, d] ;
%    H = [d, c, b, a] ;
%    H = [c, d, b, a] ;
%    H = [a, b, d, c] ;
%    false.
%    ?- hamiltoniano(andalucía,H).
%    H = [sevilla, huelva, cádiz, málaga, córdoba, jaén, almería, granada] ;
%    H = [huelva, sevilla, cádiz, málaga, córdoba, jaén, almería, granada]
%
%    ?- hamiltoniano(andalucía,_H), length(_H,N).
%    N = 8
hamiltoniano(G,H) :-
   camino(G,_,_,H),
   cubre(G,H).

% cubre(+G,+H) se verifica si el camino H cubre el grafo G (es decir, todos los
% nodos de G pertenecen a H).
cubre(G,H) :-
   igual_medida(G,H).

% igual_medida(+G,+H) se verifica si el número de nodos del camino H es igual al
% número de nodos del grafo G.
igual_medida(G,H) :-
   length(H,MH),
   nodos(G,N),
   length(N,MH).

% 2ª definición
hamiltoniano_2(G,C) :-
   nodos(G,L),
   length(L,N),
   length(C,N),
   camino(G,_,_,C).

% Comparación de eficiencia
%    ?- time(findall(_C,hamiltoniano(andalucía,_C),_L)).
%    % 119,184 inferences, 0.019 CPU in 0.019 seconds (100% CPU, 6237168 Lips)
%    true.
%
%    ?- time(findall(_C,hamiltoniano_2(andalucía,_C),_L)).
%    % 47,111 inferences, 0.009 CPU in 0.009 seconds (100% CPU, 5344800 Lips)
%    true.
