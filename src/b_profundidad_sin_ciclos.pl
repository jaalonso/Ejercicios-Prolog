% b_profundidad_sin_ciclos.pl
% Búsqueda en profundidad sin ciclos.
% José A. Alonso Jiménez <https://jaalonso.github.io>
% Sevilla, 25-mayo-2022
% ======================================================================

:- module(b_profundidad_sin_ciclos, [profundidad_sin_ciclos/1]).

% Relaciones dependientes del problema
% ====================================

% Se supone que se han definido
% 1. estado_inicial(E) que se verifica si E es el estado inicial.
% 2. estado-final(E) que se verifica si E es un estado final.
% 3. sucesor(E1,E2) que se verifica si E2 es un estado sucesor de E1.

% Procedimiento de búsqueda en profundidad en árboles
% ===================================================

% profundidad_sin_ciclos(?S) se verifica si S es una solución del
% problema mediante búsqueda en profundidad en árbol. Por ejemplo,
%    ?- [p_arbol].
%    true.
%
%    ?- profundidad_sin_ciclos(S).
%    S = [a, b, e, j]
%
%    ?- trace(estado_final,+call), profundidad_sin_ciclos(S).
%    %         estado_final/1: [call]
%     T Call: estado_final(a)
%     T Call: estado_final(b)
%     T Call: estado_final(d)
%     T Call: estado_final(h)
%     T Call: estado_final(e)
%     T Call: estado_final(i)
%     T Call: estado_final(j)
%    S = [a, b, e, j]
profundidad_sin_ciclos(S) :-
   estado_inicial(E),
   profundidad_sin_ciclos(E,S).

% profundidad_sin_ciclos(+E,?S) se verifica si S es una solución por
% búsqueda en profundidad sin ciclos a partir de E; es decir, según el
% siguiente procedimiento:
%    1. S = [E]. es una solución si E es un estado final.
%    2. S = [E|S1] es una solución si existe un E tal que
%    2.1. E1 es un sucesor de E
%    2.2. S1 es una solución por búsqueda en profundidad sin ciclos a partir de
%         E1.
profundidad_sin_ciclos(E,[E]) :-
   estado_final(E).                   % 1
profundidad_sin_ciclos(E,[E|S1]) :-   % 2
   sucesor(E,E1),                     % 2.1
   profundidad_sin_ciclos(E1,S1).     % 2.2
