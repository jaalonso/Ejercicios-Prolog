% ejercicios-tema-8.pl
% Ejercicios del tema 8 (Resolución de problemas de espacios de estados)
% José A. Alonso Jiménez <https://jaalonso.github.io>
% Sevilla, 30-mayo-2022
% ======================================================================

% ----------------------------------------------------------------------
% Ejercicio 1.1. En una mesa hay una pila de bloques y sitio para otras
% dos pilas: 
%    +---+  
%    | C |  
%    +---+  
%    | A |  
%    +---+  
%    | B |  
%    +---+    +---+    +---+
%
% Las operaciones que se pueden realizar son:
% + Mover el bloque situado en la cima de una pila a la mesa. 
% + Mover un bloque de la mesa a la cima de una pila. 
% + Mover un bloque situado en la cima de una pila a la cima de otra
%   pila. 
%
% El problema consiste en poner los bloques de la siguiente forma
%    +---+  
%    | A |  
%    +---+  
%    | B |  
%    +---+  
%    | C |  
%    +---+
% 
% En la representación del problema se consideran que los estado son
% listas de tres elementos, cada uno de los cuales es una lista con
% los elementos que la forman (el primero es la cima y el último es
% la base).
%
% Definir la relación estado_inicial(?E) que se verifica si E es el
% estado inicial. 
% ----------------------------------------------------------------------

estado_inicial([[c,a,b],[],[]]).

% ----------------------------------------------------------------------
% Ejercicio 1.2. Definir la relación estado_final(?E) que se verifica si E
% es un estado final.
% ----------------------------------------------------------------------

estado_final(E) :-
   member([a,b,c],E).

% ----------------------------------------------------------------------   
% Ejercicio 1.3. Definir la relación sucesor(+E1,?E2) que se verifica si
% E2 es un sucesor del estado E1.
% ----------------------------------------------------------------------

sucesor([[B|R],P2,P3],[R,[B|P2],P3]).
sucesor([[B|R],P2,P3],[R,P2,[B|P3]]).
sucesor([P1,[B|R],P3],[[B|P1],R,P3]).
sucesor([P1,[B|R],P3],[P1,R,[B|P3]]).
sucesor([P1,P2,[B|R]],[[B|P1],P2,R]).
sucesor([P1,P2,[B|R]],[P1,[B|P2],R]).

% ----------------------------------------------------------------------
% Ejercicio 1.4. Intentar resolver el problema mediante búsqueda en
% profundidad sin ciclos.
% ----------------------------------------------------------------------

% Con la búsqueda en profundidad sin ciclos no se encuentra solución.
%    ?- [b_profundidad_sin_ciclos].
%    true.
%    
%    ?- trace(estado_final,+call).
%    true.
%    
%    ?- profundidad_sin_ciclos(S).
%     T Call: estado_final([[c,a,b],[],[]])
%     T Call: estado_final([[a,b],[c],[]])
%     T Call: estado_final([[b],[a,c],[]])
%     T Call: estado_final([[],[b,a,c],[]])
%     T Call: estado_final([[b],[a,c],[]])
%     ...
%    ?- trace(estado_final,-call).
%    true.

% ----------------------------------------------------------------------
% Ejercicio 1.5. Resolver el problema mediante búsqueda en profundidad
% con ciclos. 
% ----------------------------------------------------------------------

% Solución del problema por búsqueda en profundidad con ciclos.
%    ?- [b_profundidad_con_ciclos].
%    true.
%
%    ?- profundidad_con_ciclos(S).
%    S = [[[c,a,b], [],      []],
%         [[a,b],   [c],     []],
%         [[b],     [a,c],   []],
%         [[],      [b,a,c], []],
%         [[],      [a,c],   [b]],
%         [[a],     [c],     [b]],
%         [[],      [c],     [a,b]],
%         [[c],     [],      [a,b]],
%         [[a,c],   [],      [b]],
%         [[c],     [a],     [b]],
%         [[],      [c,a],   [b]],
%         [[],      [a],     [c,b]],
%         [[a],     [],      [c,b]],
%         [[c,a],   [],      [b]],
%         [[b,c,a], [],      []],
%         [[c,a],   [b],     []],
%         [[a],     [c,b],   []],
%         [[],      [a,c,b], []],
%         [[],      [c,b],   [a]],
%         [[c],     [b],     [a]],
%         [[],      [b],     [c,a]],
%         [[b],     [],      [c,a]],
%         [[c,b],   [],      [a]],
%         [[b],     [c],     [a]],
%         [[],      [b,c],   [a]],
%         [[],      [c],     [b,a]],
%         [[c],     [],      [b,a]],
%         [[b,c],   [],      [a]],
%         [[a,b,c], [],      []]]

% ----------------------------------------------------------------------
% Ejercicio 1.6. Resolver el problema mediante búsqueda en anchura.
% ----------------------------------------------------------------------

% La solución del problema mediante búsqueda en anchura.
%    ?-[b_anchura].
%    true.
%
%    ?- anchura(S).
%    S = [[[c,a,b], [],      []],
%         [[a,b],   [c],     []],
%         [[b],     [c],     [a]],
%         [[],      [b,c],   [a]],
%         [[],      [a,b,c], []]]

% ----------------------------------------------------------------------
% Ejercicio 2.1. El problema de las 8 reinas consiste en colocar 8
% reinas en un tablero rectangular de dimensiones 8 por 8 de forma que
% no se encuentren más de una en la misma línea: horizontal, vertical o
% diagonal. 
%
% Los estados son listas de números que representan las ordenadas de las
% respectivas reinas. Por ejemplo, [3,5] representa que se ha colocado
% las reinas (1,5) y (2,3).
%
% Definir la relación estado_inicial(?E) que se verifica si E es el
% estado inicial.
% ----------------------------------------------------------------------

estado_inicial([]).

% ----------------------------------------------------------------------
% Ejercicio 2.2. Definir la relación estado_final(?E) que se verifica si
% E es un estado final. 
% ----------------------------------------------------------------------

estado_final(E) :-
   length(E,8).

% ----------------------------------------------------------------------
% Ejercicio 2.3. Definir la relación sucesor(+E1,?E2) que se verifica si
% E2 es un sucesor del estado E1.
% ----------------------------------------------------------------------

sucesor(E,[Y|E]) :-
   member(Y,[1,2,3,4,5,6,7,8]),
   not(member(Y,E)),
   no_ataca(Y,E).

% no_ataca(Y,E) se verifica si E = [Y_n,...,Y_1] y la reina colocada en
% (n+1,Y) no ataca a las colocadas en (1,Y_1),...,(n,Y_n).
no_ataca(Y,E) :-
   no_ataca(Y,E,1).
no_ataca(_,[],_).
no_ataca(Y,[Y1|L],D) :-
   Y1-Y =\= D,
   Y-Y1 =\= D,
   D1 is D+1,
   no_ataca(Y,L,D1).

% ----------------------------------------------------------------------
% Ejercicio 2.4. Resolver el problema por búsqueda en profundidad sin
% ciclos.
% ----------------------------------------------------------------------

% Solución
%    ?- [b_profundidad_sin_ciclos].
%    true.
%
%    ?- profundidad_sin_ciclos(S).
%    S = [[],
%         [1],
%         [5,1],
%         [8,5,1],
%         [6,8,5,1],
%         [3,6,8,5,1],
%         [7,3,6,8,5,1],
%         [2,7,3,6,8,5,1],
%         [4,2,7,3,6,8,5,1]]

% ----------------------------------------------------------------------
% Ejercicio 2.5. Resolver el problema por búsqueda en anchura.
% ----------------------------------------------------------------------

% Solución
%    ?- [b_anchura].
%    true.
%
%    ?- anchura(S).
%    S = [[],
%         [1],
%         [5,1],
%         [8,5,1],
%         [6,8,5,1],
%         [3,6,8,5,1],
%         [7,3,6,8,5,1],
%         [2,7,3,6,8,5,1],
%         [4,2,7,3,6,8,5,1]]
