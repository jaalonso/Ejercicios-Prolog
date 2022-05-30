% b_anchura.pl
% Búsqueda en anchura.
% José A. Alonso Jiménez <https://jaalonso.github.io>
% Sevilla, 25-mayo-2022
% =============================================================================

:- module(b_anchura, [anchura/1]).

% Relaciones dependientes del problema
% ====================================

% Se supone que se han definido
% 1. estado_inicial(E) que se verifica si E es el estado inicial.
% 2. estado-final(E) que se verifica si E es un estado final.
% 3. sucesor(E1,E2) que se verifica si E2 es un estado sucesor de E1.

% Búsqueda en anchura
% ===================

% Un NODO es una lista de estados [E_n,...,E_1] de forma que E_1 es el estado
% inicial y E_(i+1) es un sucesor de E_i.
%
% ABIERTOS es una lista de nodos (los nodos pendientes de analizar).

% anchura(?S) se verifica si S es una solución del problema mediante
% búsqueda en anchura; es decir, mediante el siguiente procedimiento:
% 1. Sea E el estado inicial.
% 2. La solución S es la obtenida por búsqueda en anchura con [[E]] como
%    la lista de abiertos.
anchura(S) :-
   estado_inicial(E), % 1
   anchura([[E]],S).  % 2

% anchura(+Abiertos,?S) se verifica si S es la solucion encontrada por
% búsqueda en anchura a partir de la lista de Abiertos; es decir,
% mediante el siguiente procedimiento:
% 1. Si
%    1.1. el primer elemento de Abiertos es [E|C] y
%    1.2. E es un estado final,
%    entonces
%    1.3 S es la inversa de [E|C].
% 2. Si
%    2.1. Abiertos es la lista [[E|C]|R],
%    2.2. la lista de los sucesores de E que no están en C ni en
%         Abiertos es Sucesores y
%    2.3. los nuevos abiertos, NAbiertos, es la lista obtenida añadiendo
%         dichos Sucesores a continuación de R
%    entonces
%    2.4. S es la solución obtenida por búsqueda en anchura con los
%         nuevos abiertos.
anchura([[E|C]|_],S) :-                 % 1.1
   estado_final(E),                     % 1.2
   reverse([E|C],S).                    % 1.3
anchura([N|R],S) :-
   expande([N|R],Sucesores),            % 2.2
   append(R,Sucesores,NAbiertos),       % 2.3
   anchura(NAbiertos,S).                % 2.4

% expande(+Abiertos,?Sucesores) se verifica si Sucesores es la lista de
% los sucesores del primer elemento de Abiertos que no pertenecen al
% camino que lleva a dicho elemento ni a Abiertos.
expande([[E|C]|R],Sucesores) :-
   findall([E1,E|C],
           (sucesor(E,E1),
            not(memberchk(E1,C)),
            not(memberchk([E1|_],[[E|C]|R]))),
           Sucesores).
