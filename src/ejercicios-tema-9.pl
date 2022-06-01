% ejercicios-tema-9.pl
% Ejercicios del Tema 9 (Programación lógica con restricciones).
% José A. Alonso Jiménez <https://jaalonso.github.io>
% Sevilla,  1-junio-2022
% ======================================================================

% ----------------------------------------------------------------------
% § CLP(R)                                                             
% ----------------------------------------------------------------------

:- use_module(library(clpr)).

% ----------------------------------------------------------------------
% Ejercicio 1.1. Definir en CLP(R) la relación producto(V1,V2,P) que se 
% verifica si P es el producto escalar del vector V1 por el V2. Por
% ejemplo, 
%    ?- producto([4,2],[3,7],P).
%    P = 26.0 
%    ?- producto([4,2,5],[3,7,2],P).
%    P = 36.0 
% ----------------------------------------------------------------------

producto([],[],0).
producto([X1|L1],[X2|L2],Z) :-
   producto(L1,L2,Y),
   {Z = X1*X2+Y}.

% ----------------------------------------------------------------------
% Ejercicio 1.2. Determinar X tal que el producto de [4,2] por [X,7] es
% 26.
% ----------------------------------------------------------------------

% Solución:
%    ?- producto([4,2],[X,7],26).
%    X = 3.0 ;
%    false.

% ----------------------------------------------------------------------
% Ejercicio 1.3. Determinar X e Y tales que el producto de [4,2] por
% [X,Y] es 26. 
% ----------------------------------------------------------------------

% Solución:
%    ?- producto([4,2],[X,Y],26).
%    {X=6.5-0.5*Y,_13736=2.0*Y}.

% ----------------------------------------------------------------------
% Ejercicio 1.4. Resolver el sistema de ecuaciones
%    3*X + Y   =  5
%      X + 8*Y = 17
% ----------------------------------------------------------------------

% Solución:
%    ?- producto([3,1],[X,Y],5), producto([1,8],[X,Y],17).
%    X = 0.9999999999999999,
%    Y = 2.0.

% ----------------------------------------------------------------------
% Ejercicio 2.1. Una matriz está equilibrada si cada elemento interior
% (es decir, que pertenece a una línea distinta de la primera y última
% fila y columna) es la media de sus cuatro vecinos.
% 
% Definir en CLP(R) la relación equilibrada(M) que se verifica si M es
% una matriz equilibrada. Por ejemplo, 
%    ?- equilibrada([[4,4,4],[0,Y,0],[0,0,0]]).
%    Y = 1.0 ;
%    false.
%    
%    ?- equilibrada([[4,4,4],[X,Y,0],[0,0,0]]).
%    {Y=1.0+0.25*X} ;
%    false.
%    
%    ?- equilibrada([[4,4,4],[X,1,Z],[0,0,0]]).
%    {Z= -X} ;
%    false.
% ----------------------------------------------------------------------

equilibrada([_, _]).
equilibrada([R1, R2, R3 | Rs]) :- 
   equilibrada_aux(R1, R2, R3), 
   equilibrada([R2, R3 | Rs]).
 
equilibrada_aux([_, _], [_, _], [_, _]).
equilibrada_aux([_TL, T, TR | Ts], [ML, M, MR | Ms], [_BL, B, BR | Bs]) :-
   {4*M = T + ML + MR + B}, 
   equilibrada_aux([T, TR | Ts], [M, MR | Ms], [B, BR | Bs]). 

% ----------------------------------------------------------------------
% Determinar los valores interiores de la siguiente matriz para que sea 
% equilibrada 
%    [[0,100,100,100,100,100, 0],
%     [0,  _,  _,  _,  _,  _, 0],
%     [0,  _,  _,  _,  _,  _, 0],
%     [0,  _,  _,  _,  _,  _, 0],
%     [0,  _,  _,  _,  _,  _, 0],
%     [0,  0,  0,  0,  0,  0, 0]],
% ----------------------------------------------------------------------

% Solución:
%    ?- P = [[0,100,100,100,100,100, 0],
%            [0,  _,  _,  _,  _,  _, 0],
%            [0,  _,  _,  _,  _,  _, 0],
%            [0,  _,  _,  _,  _,  _, 0],
%            [0,  _,  _,  _,  _,  _, 0],
%            [0,  0,  0,  0,  0,  0, 0]], 
%       equilibrada(P).
%    
%    P = [[0, 100,      100,      100,      100,      100,       0],
%         [0,  46.6117,  62.4778,  66.4292,  62.4778,  46.6117,  0],
%         [0,  23.9691,  36.8702,  40.7612,  36.8702,  23.9691,  0],
%         [0,  12.3945,  20.2727,  22.8752,  20.2727,  12.3945,  0],
%         [0,  5.33633,  8.9508,   10.1942,   8.9508,   5.33633, 0],
%         [0,  0,        0,         0,        0,        0,       0]] ;
%    false.

% ----------------------------------------------------------------------
% § CLP(FD)
% ----------------------------------------------------------------------

:- use_module(library(clpfd)).

% ----------------------------------------------------------------------
% Ejercicio 3. Definir en CLP(FD) la relación solución tal que
% solución([C,U,A,T,R,O],[V,E,I,N,T,E]) que se verifica si el valor de
% cada letra es un dígito distinto y se cumple la ecuación
%    CUATRO * 5 = VEINTE
%
% Nota: La solución se puede obtener mediante el objetivo
% solución(L1,L2).  
% ----------------------------------------------------------------------

solución([C,U,A,T,R,O],[V,E,I,N,T,E]) :-
   Vars = [C,U,A,T,R,O,V,E,I,N],
   Vars ins 0..9,                                 % Los valores son los dígitos
   all_distinct(Vars),                            % Los valores son distintos
      (100000*C+10000*U+1000*A+100*T+10*R+O) * 5
   #=  100000*V+10000*E+1000*I+100*N+10*T+E,
   label(Vars).

% Cálculo de la solución
%    ?- solución([C,U,A,T,R,O],[V,E,I,N,T,E]).
%    C = 1,
%    U = 7,
%    A = 0,
%    T = 4,
%    R = 6,
%    O = 9,
%    V = 8,
%    E = 5,
%    I = 2,
%    N = 3 ;
%    false.
%    
%    ?- solución(L1, L2).
%    L1 = [1,7,0,4,6,9],
%    L2 = [8,5,2,3,4,5] ;
%    false.

% ----------------------------------------------------------------------
% Ejercicio 4.1. (Problema de la mochila) Un contrabandista tiene una
% mochila de capacidad limitada, supongamos que a 9 unidades de peso. Él
% puede hacer contrabando de botellas de whisky de 4 unidades de peso,
% botes de colonia de 3 unidades de peso y cartones de tabaco de 2
% unidades de peso. Las ganancias en las botellas de whisky, botes de
% colonia y cartones de tabaco son de 15, 10 y 7 euros,
% respectivamente.
%
% Definir en CLP(FD) la relación viaje_rentable(+N,?W,?C,?T,?G) que se
% verifique si con W unidades de whisky, C de colonia y T de tabacos se
% obtiene una ganancia G que es mayor o igual que N. Por ejemplo,  
%    ?- viaje_rentable(30, W, C, T, G).
%    W = 0,
%    C = 1,
%    T = 3,
%    G = 31 ;
%    W = T, T = 0,
%    C = 3,
%    G = 30 ;
%    W = C, C = T, T = 1,
%    G = 32 ;
%    W = 2,
%    C = T, T = 0,
%    G = 30.
% ----------------------------------------------------------------------

viaje_rentable(N,W,C,T,G) :-
   [W,C,T] ins 0..9, 
   4*W + 3*C + 2*T #=< 9, 
   G #= 15*W + 10*C + 7*T,
   G #>= N,
   label([W,C,T]).

% ----------------------------------------------------------------------
% Ejercicio 4.2. Definir en CLP(FD) la relación viaje_óptimo(?W,?C,?T)
% que se verifique si con W unidades de whisky C de colonia y T de
% tabacos se obtiene la máxima ganancia. Por ejemplo, 
%    ?- viaje_óptimo(W,C,T).
%    W = C, C = T, T = 1 ;
%    false.
% ----------------------------------------------------------------------

viaje_óptimo(W,C,T) :-
   viaje_rentable(_,W,C,T,G),
   not((viaje_rentable(_,_,_,_,G1), G1 #> G)).

% ----------------------------------------------------------------------
% Ejercicio 5. Se tiene que realizar simultáneamente 4 tareas y se
% dispone de cuatro empresas que han realizado un presupuesto para cada
% tarea resumido en la siguiente tabla
%       T1 T2 T3 T4
%    E1 3  9  7  6
%    E2 2  8  5  9
%    E3 6  7  3  8
%    E4 7  9  4  7
% 
% Definir en CLP(FD) la relación asignación_óptima(M,L) que se verifique
% si L es la asignación de las tareas que hace mínimo el coste
% total. Por ejemplo, 
%    ?- asignación_óptima([[3,9,7,6],
%                          [2,8,5,9],
%                          [6,7,3,8],
%                          [7,9,4,7]],
%                         [T1,T2,T3,T4]).
%    T1 = 3
%    T2 = 4
%    T3 = 1
%    T4 = 2
% ----------------------------------------------------------------------

asignación([E1,E2,E3,E4],[T1,T2,T3,T4],C) :-
   L = [T1,T2,T3,T4],
   L ins 1..4,           % Ti=j significa que el trabajo i se asigna a j
   all_distinct(L),
   nth1(T1,E1,CT1),
   nth1(T2,E2,CT2),
   nth1(T3,E3,CT3),
   nth1(T4,E4,CT4),
   C #= CT1+CT2+CT3+CT4,
   label(L).

asignación_óptima([E1,E2,E3,E4],[T1,T2,T3,T4]) :-
   asignación([E1,E2,E3,E4],[T1,T2,T3,T4],C),
   not((asignación([E1,E2,E3,E4],[_,_,_,_],C1), C1 > C)).
