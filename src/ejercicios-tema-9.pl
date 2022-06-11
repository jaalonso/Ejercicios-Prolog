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

% ----------------------------------------------------------------------
% § Ejercicios de temas anteriores
% ----------------------------------------------------------------------

% Nota. Es esta sección se definirán relaciones estudiadas en temas
% anteriores pero usando restricciones.

% ----------------------------------------------------------------------
% Ejercicio 6. Definir la relación factorial(X,Y) se verifica si Y es el
% factorial de X. Por ejemplo,
%    ?- factorial(3,Y).
%    Y = 6 ;
%    false.
%    
%    ?- factorial(X,6).
%    X = 3 ;
%    false.
%    
%    ?- factorial(X,7).
%    false.
% ----------------------------------------------------------------------

factorial(0,1).
factorial(X,Y) :-
   X #> 0,
   A #= X - 1,
   Y #= X * B,
   factorial(A,B).

% ----------------------------------------------------------------------
% Ejercicio 7. Definir la relación máximo(X,Y,Z) que se verifica si Z es
% el máximo de los números X e ~Y. Por ejemplo,
%    ?- máximo(2,3,Z).
%    Z = 3.
%    
%    ?- máximo(2,Y,3).
%    Y = 3.
%    
%    ?- máximo(X,3,3).
%    X in inf..3.
% ----------------------------------------------------------------------

máximo(X,Y,Z) :-
   Z #= max(X,Y).

% ----------------------------------------------------------------------
% Ejercicio 8. (Algoritmo de Euclides). Dados dos enteros positivos X e
% Y, el máximo común divisor (mcd) D puede obtenerse de la siguiente
% manera: 
% + Si X e Y son iguales, entonces D es igual a X.
% + Si X < Y, entonces D es igual al máximo común divisor de X y la
%   diferencia Y-X. 
% + Si Y < X entonces D es igual al máximo común divisor de X y la
%   diferencia X-Y. 
% 
% Definir el predicado mcd(X,Y,D) que se verifica si D es el máximo
% común divisor de los enteros positivos X e Y. Por ejemplo,
%    ?- mcd(6,10,D).
%    D = 2 
% ----------------------------------------------------------------------

mcd(X,X,X).
mcd(X,Y,Z) :-
   X  #< Y,
   Y1 #= Y - X,
   mcd(X,Y1,Z).
mcd(X,Y,Z) :-
   X #> Y,
   mcd(Y,X,Z).

% ----------------------------------------------------------------------
% Ejercicio 9.1. Supongamos que los cuadros del tablero de ajedrez los
% representamos por pares de números [X,Y] con X e Y entre 1 y 8. 
% 
% Definir la relación salta(+C1,?C2) que se verifica si el caballo puede 
% pasar en un movimiento del cuadrado C1 al cuadrado C2. Por ejemplo,
%    ?- salta([1,1],S).
%    S = [3,2] ;
%    S = [2,3] ;
%    false.
%    
%    ?- salta(X,[3,2]).
%    X = [1,1] ;
%    X = [1,3] ;
%    X = [5,1] ;
%    X = [5,3] ;
%    X = [2,0] ;
%    X = [2,4] ;
%    X = [4,0] ;
%    X = [4,4].
% ----------------------------------------------------------------------

salta([X,Y],[X1,Y1]) :-
   [X,Y,X1,Y1] ins 1..8,
   dxy(Dx,Dy),
   X1 #= X+Dx,
   Y1 #= Y+Dy.

% dxy(?X,?Y) se verifica si un caballo puede moverse X espacios
% horizontales e Y verticales.
dxy(2,1).
dxy(2,-1).
dxy(-2,1).
dxy(-2,-1).
dxy(1,2).
dxy(1,-2).
dxy(-1,2).
dxy(-1,-2).

% ----------------------------------------------------------------------
% Ejercicio 9.2. Definir la relación camino(L) que se verifica si L es
% una lista de cuadrados representando el camino recorrido por un
% caballo sobre un tablero vacío. Por ejemplo, 
%    ?- camino([[1,1]|C]).
%    C = [] ;
%    C = [[3,2]] ;
%    C = [[3,2],[5,3]] ;
%    C = [[3,2],[5,3],[7,4]] 
%    
%    ?- camino(L), append(_L1,[[5,3]],L).
%    L = [[5,3]] ;
%    L = [[3,2],[5,3]] ;
%    L = [[3,4],[5,3]] ;
%    L = [[7,2],[5,3]] ;
%    L = [[7,4],[5,3]]
%    
%    ?- camino(L), length(L,4), append(_L1,[[5,3]],L).
%    L = [[2,3],[1,1],[3,2],[5,3]] ;
%    L = [[3,4],[1,3],[3,2],[5,3]] 
% ----------------------------------------------------------------------

camino([_]).
camino([C1,C2|L]) :-
   camino([C2|L]),
   salta(C1,C2),
   \+ member(C1,L),
   \+ member(C2,L).

% ----------------------------------------------------------------------
% Ejercicio 10. Definir la relación factores(X,Y,Z) que se verifica si
% el producto de X e Y es Z. Por ejemplo,
%    ?- factores(2,3,Z).
%    Z = 6.
%    
%    ?- factores(2,Y,6).
%    Y = 3.
%    
%    ?- factores(X,3,6).
%    X = 2.
%    
%    ?- factores(2,Y,7).
%    false.
%    
%    ?- factores(X,Y,6).
%    X = -6,
%    Y = -1 ;
%    X = -3,
%    Y = -2 ;
%    X = -2,
%    Y = -3 ;
%    X = -1,
%    Y = -6 ;
%    X = 1,
%    Y = 6 ;
%    X = 2,
%    Y = 3 ;
%    X = 3,
%    Y = 2 ;
%    X = 6,
%    Y = 1.
% ----------------------------------------------------------------------

factores(X,Y,Z) :-
   Z #= X * Y,
   label([X,Y]).

% ----------------------------------------------------------------------
% Ejercicio 11. Definir la relación suma_segura(X,Y,Z) se verifica si X
% e Y son enteros y Z es la suma de X e Y. Por ejemplo,
%    ?- suma_segura(2,3,Z).
%    Z = 5.
%    
%    ?- suma_segura(2,Y,5).
%    Y = 3.
%    
%    ?- suma_segura(X,3,5).
%    X = 2.
%    
%    ?- suma_segura(X,Y,3).
%    X = 0,
%    Y = 3 ;
%    X = 1,
%    Y = 2 ;
%    X = 2,
%    Y = 1 ;
%    X = 3,
%    Y = 0.
% ----------------------------------------------------------------------

suma_segura(X,Y,Z) :-
   [X,Y] ins 0..sup,
   Z #= X+Y,
   label([X,Y]).

% ----------------------------------------------------------------------
% Ejercicio 12. El problema de los cuadrados mágicos consiste en colocar
% los números 1,2,3,4,5,6,7,8,9 en un cuadrado 3x3 de forma que todas
% las líneas (filas, columnas y diagonales) sumen igual; es decir,
% buscar los valores para las variables A, B ,C, D, E, F, G, H, I.
%    +---+---+---+
%    | A | B | C |
%    +---+---+---+
%    | D | E | F |
%    +---+---+---+
%    | G | H | I |
%    +---+---+---+
% tales que
%    {A,B,C,D,E,F,G,H,I} = {1,2,3,4,5,6,7,8,9},
%    A+B+C = 15,
%    D+E+F = 15,
%    G+H+I = 15,
%    A+D+G = 15,
%    B+E+H = 15,
%    C+F+I = 15,
%    A+E+I = 15,
%    C+E+G = 15.
%
% Definir la relación cuadrado(L) que se verifica si L es una solución
% del problema de los cuadrados mágicos. Por ejemplo, 
%    ?- cuadrado(L).
%    L = [2,7,6,9,5,1,4,3,8] ;
%    L = [2,9,4,7,5,3,6,1,8] 
%    ?- findall(_X,cuadrado(_X),_L),length(_L,N).
%    N = 8.
% ----------------------------------------------------------------------

% 1ª solución
cuadrado([A,B,C,D,E,F,G,H,I]) :-
   [A,B,C,D,E,F,G,H,I] ins 1..9,
   all_distinct([A,B,C,D,E,F,G,H,I]),
   A+B+C #= 15,
   D+E+F #= 15,
   G+H+I #= 15,
   A+D+G #= 15,
   B+E+H #= 15,
   C+F+I #= 15,
   A+E+I #= 15,
   C+E+G #= 15,
   label([A,B,C,D,E,F,G,H,I]).

% 2ª solución
cuadrado_2(Sol):-
  Sol = [A,B,C,D,E,F,G,H,I],
  Sol ins 1..9,
  all_distinct(Sol),
  maplist(#=(15),
    [A+B+C,
     D+E+F,
     G+H+I,
     A+D+G,
     B+E+H,
     C+F+I,
     A+E+I,
     C+E+G]),
  label(Sol).

% ----------------------------------------------------------------------
% Ejercicio 13, La sucesión de Fibonacci es 1, 1, 2, 3, 5, 8, ... y está
% definida por 
%    f(1) = 1
%    f(2) = 1
%    f(n) = f(n-1)+f(n-2), si n > 2
%
% Definir la relación fibonacci(N,X) que se verifica si X es el N-ésimo
% término de la sucesión de Fibonacci. Por ejemplo,
%    ?- fibonacci(6,X).
%    X = 8 
%    
%    ?- fibonacci(X,8).
%    X = 6 
%    
%    ?- fibonacci(X,9).
%    false.
% ----------------------------------------------------------------------

fibonacci(1,1).
fibonacci(2,1).
fibonacci(N,F) :-
   N #> 2,
   F #>= 2,
   N1 #= N-1,
   N2 #= N-2,
   F #= F1 + F2,
   [F1,F2] ins 1..sup,
   fibonacci(N1,F1),
   fibonacci(N2,F2).

% ----------------------------------------------------------------------
% Ejercicio 14. El problema de las N reinas consiste en colocar N reinas
% en un tablero rectangular de dimensiones N por N de forma que no se
% encuentren más de una en la misma línea: horizontal, vertical o
% diagonal.
%
% Definir la relación reinas(N,S) que se verifica si S es una solución
% del problema de las N reinas. Por ejemplo,  
%    ?- reinas(4, Rs).
%    Rs = [2,4,1,3] ;
%    Rs = [3,1,4,2] 
%    
%    ?- reinas(8, Rs).
%    Rs = [1,5,8,6,3,7,2,4]
%    
%    ?- findall(S,reinas(8,S),_L), length(_L,N).
%    N = 92.
% ----------------------------------------------------------------------

reinas(N, Rs) :-
   length(Rs, N),
   Rs ins 1..N,
   seguras(Rs),
   label(Rs).

seguras([]).
seguras([R|Rs]) :-
   seguras(Rs, R, 1),
   seguras(Rs).

seguras([], _, _).
seguras([R|Rs], R0, D0) :-
   R0 #\= R,
   abs(R0 - R) #\= D0,
   D1 #= D0 + 1,
   seguras(Rs, R0, D1).

% ----------------------------------------------------------------------
% Ejercicio 15.1. (Coloreado de mapas). Se considera el siguiente mapa
%    +----------+----------+       
%    |    a     |     b    |       
%    +----+-----+-----+----+       
%    |    |           |    |       
%    | c  |     d     | e  |       
%    |    |           |    |       
%    +----+-----+-----+----+       
%    |    f     |     g    |       
%    +----------+----------+       
%
% Definir la relación coloración1(Rs) que se verifica si Rs es una de
% números que representa los colores de los regiones de Rs tal que las
% regiones vecinas tengan colores distintos. Por ejemplo,  
%    ?- coloración1(Rs).
%    Rs = [1,2,2,3,1,1,2] ;
%    Rs = [1,2,2,3,1,1,4] 
% ----------------------------------------------------------------------

coloración1(Rs) :-
   Rs = [A,B,C,D,E,F,G],
   distinta(A,[B,C,D]),
   distinta(B,[A,D,E]),
   distinta(C,[A,D,F]),
   distinta(D,[A,B,C,E,F,G]),
   distinta(E,[B,D,G]),
   distinta(F,[C,D,G]),
   distinta(G,[D,E,F]),
   label(Rs).

distinta(X,Ys) :-
   X in 1..4,
   Ys ins 1..4,
   maplist(#\=(X),Ys),
   label([X|Ys]).

% ----------------------------------------------------------------------
% Ejercicio 15.2. Se considera el siguiente mapa
%    +----+-----+-----+----+
%    | a  |  b  |  c  | d  |
%    +----+-----+-----+----+
%    | e  |           | f  |
%    +----+     k     +----+
%    | g  |           | h  |
%    +----+-----+-----+----+
%    |    i     |     j    |
%    +----------+----------+
%
% Definir la relación coloración2(Rs) que se verifica si Rs es una de
% números que representa los colores de los regiones de Rs tal que las
% regiones vecinas tengan colores distintos. Por ejemplo,  
%    ?- coloración2(Rs).
%    Rs = [1,2,1,2,3,3,1,1,2,3,4] ;
%    Rs = [1,2,1,2,3,3,1,1,3,2,4] 
% ----------------------------------------------------------------------

coloración2(Rs) :-
   Rs = [A,B,C,D,E,F,G,H,I,J,K],
   distinta(A,[B,E,K]),
   distinta(B,[A,C,E,K]),
   distinta(C,[B,D,F,K]),
   distinta(D,[C,F,K]),
   distinta(E,[A,B,G,K]),
   distinta(F,[C,D,H,K]),
   distinta(G,[E,I,K]),
   distinta(H,[F,J,K]),
   distinta(I,[G,J,K]),
   distinta(J,[I,H,K]),
   distinta(K,[A,B,C,D,E,F,G,H,I,J]),
   label(Rs).

% ----------------------------------------------------------------------
% Ejercicio 16. El problema de los animales es el siguiente:
%    Un granjero tiene pollos y vacas. Sabiendo que tiene A animales, y
%    los animales tienen P patas en total, ¿cuántos pollos tiene el
%    granjero?
% 
% Definir la relación animales(A,P,X,Y) que se verifica si X es el
% número de pollos e Y es el número de vacas sabiendo que hay A animales
% que tienen P patas. Por ejemplo, 
%    ?- animales(30,74,Pollos,Vacas).
%    Pollos = 23,
%    Vacas = 7.
% ----------------------------------------------------------------------

animales(A,P,Pollos,Vacas) :-
   Pollos + Vacas #= A,
   2*Pollos + 4*Vacas #= P,
   [Pollos, Vacas] ins 0..sup.

% ----------------------------------------------------------------------
% Ejercicio 17. Un árbol binario es vacío o consta de tres partes: la
% raíz (que debe de ser un número positivo), el subárbol izquierdo (que
% debe ser un árbol binario) y el subárbol derecho (que debe ser un
% árbol binario). Usaremos la siguiente representación 
% + nil representa el árbol vacío
% + t(I,R,D) representa el árbol de la raíz R, subárbol izquierdo I y
%   subárbol derecho D.
% Por ejemplo, el término t(t(nil,2,nil),1,t(t(nil,4,nil),3,nil))
% representa el árbol 
%        1                                   
%      /   \                                 
%     2     3                                
%          /                                 
%         4                                  
% 
% Definir la relación máximo(+T,-X) que se verifica si X es el máximo de
% los nodos del árbol T. Por ejemplo, 
%    ?- máximo(nil,N).
%    N = 0. 
%    ?- máximo(t(nil,2,nil),N).
%    N = 2. 
%    ?- máximo(t(t(nil,2,nil),3,nil),N).
%    N = 3.
% ----------------------------------------------------------------------

máximo(nil,0).
máximo(t(I,R,D),M):-
   máximo(I,MI),
   máximo(D,MD),
   M1 #= max(MI,MD),
   M #= max(R,M1).

% ----------------------------------------------------------------------
% Ejercicio 18. Definir la relación cota_superior(+L,N) que se verifique
% si N es una cota superior de L (es decir, todos los elementos de L son
% menores o iguales que N). Por ejemplo,
%    ?- cota_superior([1,5,3],7).
%    true.
%    
%    ?- cota_superior([1,5,3],5).
%    true.
%    
%    ?- cota_superior([1,5,3],4).
%    false.
%    
%    ?- cota_superior([1,5,3],X).
%    X in 5..sup.
% ----------------------------------------------------------------------

% 1ª solución
% ===========

cota_superior_1([],_).
cota_superior_1([X|L],N) :-
   X #=< N,
   cota_superior_1(L,N).

% 2ª solución
% ===========

cota_superior_2(L,N) :-
   max_list(L,M),
   N #>= M.
