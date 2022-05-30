% b_profundidad_con_ciclos.pl
% Búsqueda en profundidad con detección de ciclos.
% José A. Alonso Jiménez <https://jaalonso.github.io>
% Sevilla, 25-mayo-2022
% ======================================================================

:- module(b_profundidad_con_ciclos, [profundidad_con_ciclos/1]).

% Relaciones dependientes del problema
% ====================================

% Se supone que se han definido
% 1. estado_inicial(E) que se verifica si E es el estado inicial.
% 2. estado-final(E) que se verifica si E es un estado final.
% 3. sucesor(E1,E2) que se verifica si E2 es un estado sucesor de E1.

% Búsqueda en profundidad en grafos
% =================================

% Un NODO es una lista de estados [E_n,...,E_1] de forma que E_1 es el estado
% inicial y E_(i+1) es un sucesor de E_i.

% profundidad_con_ciclos(?S) se verifica si S es una solución del
% problema mediante búsqueda en profundidad en grafo; es decir, S es una
% solución obtenida por búsqueda en profundidad en grafo a partir del
% nodo cuyo único elemento es el estado inicial.
profundidad_con_ciclos(Sol) :-
   estado_inicial(E),
   profundidad_con_ciclos([E],Sol).

% profundidad_con_ciclos(+N,?S) se verifica si S es una solución del
% problema a partir del nodo N (es decir S=[E_1,...,E_m] donde
% N=[E_j,E_(j-1),...,E_1] , E_n es un estado final y E_(i+1) es un
% sucesor de Ei), encontrada por búsqueda en profundidad; es decir,
% mediante el siguiente procedimiento:
% 1. Si el primer elemento de N es un estado final, entonces S es la
%    inversa de N.
% 2. Si N=[E|C] y E1 un sucesor de E que no ha sido visitado (es decir,
%    que no pertenece a C) y tal que existe una solución, S, a partir de
%    [E1,E|C].
profundidad_con_ciclos([E|C],S) :-
   estado_final(E),
   reverse([E|C],S).
profundidad_con_ciclos([E|C],S) :-
   sucesor(E,E1),
   not(memberchk(E1,C)),
   profundidad_con_ciclos([E1,E|C],S).
