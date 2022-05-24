% representacion_de_grafos.pl
% Representación de grafos.
% José A. Alonso Jiménez <https://jaalonso.github.io>
% Sevilla, 23-mayo-2022
% =============================================================================

% Un grafo se representa mediante la definición del predicado grafo(G,A) que se
% verifica si G es el nombre del grafo y A es la lista de arcos de G (cada uno
% representado mediante un término X-Y). Por  ejemplo, la representación del
% siguiente grafo
%        b
%      / | \
%     /  |  \
%    a   |   c
%        |  /
%        | /
%        |/
%        d
% es
grafo(ejemplo1,[a-b,b-c,b-d,c-d]).
% y la representación del mapa de Andalucía es
grafo(andalucía, [almería-granada,
                  almería-jaén,
                  cádiz-huelva,
                  cádiz-málaga,
                  cádiz-sevilla,
                  córdoba-granada,
                  córdoba-jaén,
                  córdoba-málaga,
                  córdoba-sevilla,
                  granada-jaén,
                  granada-málaga,
                  huelva-sevilla,
                  málaga-sevilla]).

:- multifile grafo/2.

% arcos(G,L) se verifica si L es la lista de los arcos del grafo G, de forma que
% el arco de extremo X e Y se represente por X-Y. Por ejemplo,
%    ?- arcos(ejemplo1,L).
%    L = [a-b, b-c, b-d, c-d].
%    ?- arcos(andalucía,L).
%    L = [almería-granada,
%         almería-jaén,
%         cádiz-huelva,
%         cádiz-málaga,
%         cádiz-sevilla,
%         córdoba-granada,
%         córdoba-jaén,
%         córdoba-málaga,
%         ... - ...|...].
arcos(G,A) :-
   grafo(G,A).

% adyacente(G,X,Y) se verifica si los nodos X e Y son adyacentes en el grafo
% G. Por ejemplo,
%    ?- adyacente(ejemplo1,d,X).
%    X = b ;
%    X = c.
%    ?- adyacente(andalucía,almería,X).
%    X = granada ;
%    X = jaén ;
%    false.
adyacente(G,X,Y) :-
   grafo(G,A),
   (member(X-Y,A) ; member(Y-X,A)).

% nodos(G,L) se verifica si L es la lista de los nodos del grafo G. Por ejemplo,
%    ?- nodos(ejemplo1,L).
%    L = [a, b, c, d].
%    ?- nodos(andalucía,L).
%    L = [almería, cádiz, córdoba, granada, huelva, jaén, málaga, sevilla].
nodos(G,L) :-
   setof(X,Y^adyacente(G,X,Y),L).

% nodo(G,N) se verifica si N es un nodo de G. Por ejemplo,
%    ?- nodo(ejemplo1,N).
%    N = a ;
%    N = b ;
%    N = c ;
%    N = d.
%    ?- nodo(andalucía,N).
%    N = almería ;
%    N = cádiz ;
%    N = córdoba ;
%    N = granada ;
%    N = huelva ;
%    N = jaén ;
%    N = málaga ;
%    N = sevilla.
nodo(G,N) :-
   nodos(G,L),
   member(N,L).
