% ejercicios-tema-3.pl
% Ejercicios del tema 3 (Estructuras).
% José A. Alonso Jiménez <https://jaalonso.github.io>
% Sevilla, 19-mayo-2022
% ======================================================================

% ----------------------------------------------------------------------
% Ejercicio 1. En este ejercicio vamos a considerar el programa
% familia.pl del tema 3 que se incluye a continuación
% ----------------------------------------------------------------------

familia(persona([tomás,garcía,pérez],
                fecha(7,mayo,1990),
                trabajo(profesor,75)),
        persona([ana,lópez,ruiz],
                fecha(10,marzo,1972),
                trabajo(médica,100)),
        [ persona([juan,garcía,lópez],
                  fecha(5,enero,1990),
                  estudiante),
          persona([maría,garcía,lópez],
                  fecha(12,abril,1992),
                  estudiante) ]).
familia(persona([josé,pérez,ruiz],
                fecha(6,marzo,1973),
                trabajo(pintor,150)),
        persona([luisa,gálvez,pérez],
                fecha(12,mayo,1974),
                trabajo(médica,100)),
        [ persona([juan_luis,pérez,pérez],
                  fecha(5,febrero,2000),
                  estudiante),
          persona([maría_josé,pérez,pérez],
                  fecha(12,junio,2002),
                  estudiante),
          persona([josé_maría,pérez,pérez],
                  fecha(12,julio,2004),
                  estudiante) ]).

casado(X) :-
   familia(X,_,_).

casada(X) :-
   familia(_,X,_).

% ----------------------------------------------------------------------
% Ejercicio 1.1. Definir la relación hijo(X) que se verifique si X
% figura en alguna lista de hijos.
% ----------------------------------------------------------------------

hijo(X) :-
   familia(_,_,L),
   member(X,L).

% ----------------------------------------------------------------------
% Ejercicio 1.2. Calcular los hijos de las familias.
% ----------------------------------------------------------------------

% La consulta es
%    ?- hijo(X).
%    X = persona([juan, garcía, lópez], fecha(5, enero, 1990), estudiante) ;
%    X = persona([maría, garcía, lópez], fecha(12, abril, 1992), estudiante) ;
%    X = persona([juan_luis, pérez, pérez], fecha(5, febrero, 2000), estudiante) ;
%    X = persona([maría_josé, pérez, pérez], fecha(12, junio, 2002), estudiante) ;
%    X = persona([josé_maría, pérez, pérez], fecha(12, julio, 2004), estudiante).

% ----------------------------------------------------------------------
% Ejercicio 1.3. Definir la relación existe(X) que se verifique si X es
% una persona existente en la base de datos.
% ----------------------------------------------------------------------

existe(X) :-
   casado(X);
   casada(X);
   hijo(X).

% ----------------------------------------------------------------------
% Ejercicio 1.4. Hacer una pregunta tal que que las respuestas sean las
% listas de la forma [nombre, apellido1, apellido2] de todas las
% personas que existen en las familias.
% ----------------------------------------------------------------------

% La consulta es
%    ?- existe(persona(X,_,_)).
%    X = [tomás, garcía, pérez] ;
%    X = [josé, pérez, ruiz] ;
%    X = [ana, lópez, ruiz] ;
%    X = [luisa, gálvez, pérez] ;
%    X = [juan, garcía, lópez] ;
%    X = [maría, garcía, lópez] ;
%    X = [juan_luis, pérez, pérez] ;
%    X = [maría_josé, pérez, pérez] ;
%    X = [josé_maría, pérez, pérez].

% ----------------------------------------------------------------------
% Ejercicio 1.5. Determinar todos los estudiantes nacidos antes de
% 2003.
% ----------------------------------------------------------------------

% La consulta es
%    ?- existe(persona(X,fecha(_,_,Año),estudiante)), Año < 2003.
%    X = [juan, garcía, lópez],
%    Año = 1990 ;
%    X = [maría, garcía, lópez],
%    Año = 1992 ;
%    X = [juan_luis, pérez, pérez],
%    Año = 2000 ;
%    X = [maría_josé, pérez, pérez],
%    Año = 2002 ;
%    false.

% ----------------------------------------------------------------------
% Ejercicio 1.6. Definir la relación fecha_de_nacimiento(X,Y) de forma
% que si X  es una persona, entonces Y es su fecha de nacimiento.
% ----------------------------------------------------------------------

fecha_de_nacimiento(persona(_,Y,_),Y).

% ----------------------------------------------------------------------
% Ejercicio 1.7. Buscar todos los hijos nacidos en 2002.
% ----------------------------------------------------------------------

% La consulta es
%    ?- hijo(X), fecha_de_nacimiento(X,fecha(_,_,2002)).
%    X = persona([maría_josé, pérez, pérez], fecha(12, junio, 2002), estudiante)
%    false.

% ----------------------------------------------------------------------
% Ejercicio 1.8. Definir la relación sueldo(X,Y) que se verifique si el
% sueldo de la persona X es Y.
% ----------------------------------------------------------------------

sueldo(persona(_,_,trabajo(_,Y)),Y).
sueldo(persona(_,_,estudiante),0).

% ----------------------------------------------------------------------
% Ejercicio 1.9. Buscar todas las personas nacidas antes de 1974 cuyo
% sueldo sea superior a 72 euros.
% ----------------------------------------------------------------------

% La consulta es
%    ?- existe(X),
%       fecha_de_nacimiento(X,fecha(_,_,Año)),
%       Año < 1974,
%       sueldo(X,Y),
%       Y > 72.
%    X = persona([josé, pérez, ruiz], fecha(6, marzo, 1973), trabajo(pintor, 150)),
%    Año = 1973,
%    Y = 150 ;
%    X = persona([ana, lópez, ruiz], fecha(10, marzo, 1972), trabajo(médica, 100)),
%    Año = 1972,
%    Y = 100 ;
%    false.

% ----------------------------------------------------------------------
% Ejercicio 1.10. Definir la relación total(L,Y) que se verifique si Y
% es la suma de los sueldos de las personas de la lista L.
% ----------------------------------------------------------------------

total([],0).
total([X|L],Y) :-
   sueldo(X,Y1),
   total(L,Y2),
   Y is Y1 + Y2.

% ----------------------------------------------------------------------
% Ejercicio 1.11. Calcular los ingresos totales de cada familia.
% ----------------------------------------------------------------------

% La consulta es
%    ?- familia(X,Y,Z), total([X,Y|Z],Total).
%    X = persona([tomás, garcía, pérez],
%                fecha(7, mayo, 1990),
%                trabajo(profesor, 75)),
%    Y = persona([ana, lópez, ruiz],
%                fecha(10, marzo, 1972),
%                trabajo(médica, 100)),
%    Z = [persona([juan, garcía, lópez],
%                 fecha(5, enero, 1990),
%                 estudiante),
%         persona([maría, garcía, lópez],
%                 fecha(12, abril, 1992),
%                 estudiante)],
%    Total = 175 ;
%    X = persona([josé, pérez, ruiz],
%                fecha(6, marzo, 1973),
%                trabajo(pintor, 150)),
%    Y = persona([luisa, gálvez, pérez],
%                fecha(12, mayo, 1974),
%                trabajo(médica, 100)),
%    Z = [persona([juan_luis, pérez, pérez],
%                 fecha(5, febrero, 2000),
%                 estudiante),
%         persona([maría_josé, pérez, pérez],
%                 fecha(12, junio, 2002),
%                 estudiante),
%         persona([josé_maría, pérez, pérez],
%                 fecha(12, julio, 2004),
%                 estudiante)],
%    Total = 250 ;
%    false.

% ----------------------------------------------------------------------
% Ejercicio 2. En este ejercicio consideraremos el programa automata.pl
% del tema 3 que se incluye a continuación
% ----------------------------------------------------------------------

final(e3).

trans(e1,a,e1).
trans(e1,a,e2).
trans(e1,b,e1).
trans(e2,b,e3).
trans(e3,b,e4).

nulo(e2,e4).
nulo(e3,e1).

% ----------------------------------------------------------------------
% Ejercicio 2.1. Definir la relación acepta_acotada(S,L,N) que se
% verifique si el autómata, a partir del estado S, acepta la lista L y
% la longitud de L es N.
% ----------------------------------------------------------------------

% 1ª solución
% ===========

acepta_acotada_a(E,L,N) :-
   length(L,N),
   acepta_acotada_a_aux(E,L).
acepta_acotada_a_aux(E,[]) :-
   final(E).
acepta_acotada_a_aux(E,[X|L]) :-
   trans(E,X,E1),
   acepta_acotada_a_aux(E1,L).
acepta_acotada_a_aux(E,L) :-
   nulo(E,E1),
   acepta_acotada_a_aux(E1,L).

% 2ª solución
% ===========

acepta_acotada_b(E,[],0) :-
   final(E).
acepta_acotada_b(E,[X|L],N) :-
   N > 0,
   trans(E,X,E1),
   M is N -1,
   acepta_acotada_b(E1,L,M).
acepta_acotada_b(E,L,N) :-
   nulo(E,E1),
   acepta_acotada_b(E1,L,N).

% ----------------------------------------------------------------------
% Ejercicio 2.2. Buscar las cadenas aceptadas a partir de e1 con
% longitud 3.
% ----------------------------------------------------------------------

% La consulta es
%    ?- acepta_acotada_a(e1,L,3).
%    L = [a, a, b] ;
%    L = [b, a, b] ;
%    false.
%
%    ?- acepta_acotada_b(e1,L,3).
%    L = [a, a, b] ;
%    L = [b, a, b] ;
%    false.

% ----------------------------------------------------------------------
% Ejercicio 2.3. Definir la relación acepta_acotada_2(S,L,N) que se
% verifique si el autómata, a partir del estado S, acepta la lista L y
% la longitud de L es menor o igual que N.
% ----------------------------------------------------------------------

% 1ª solución
% ===========

acepta_acotada_2_a(E,L,N) :-
   between(0,N,M),
   length(L,M),
   acepta_acotada_a_aux(E,L).

% 2ª solución
% ===========

acepta_acotada_2_b(E,[],_N) :-
   final(E).
acepta_acotada_2_b(E,[X|L],N) :-
   N > 0,
   trans(E,X,E1),
   M is N-1,
   acepta_acotada_2_b(E1,L,M).
acepta_acotada_2_b(E,L,N) :-
   nulo(E,E1),
   acepta_acotada_2_b(E1,L,N).

% ----------------------------------------------------------------------
% Ejercicio 2.4. Buscar las cadenas aceptadas a partir de e1 con
% longitud menor o igual que 3.
% ----------------------------------------------------------------------

% La consulta es
%    ?- acepta_acotada_2_a(e1,L,3).
%    L = [a, b] ;
%    L = [a, a, b] ;
%    L = [b, a, b] ;
%    false.
%
%    ?- acepta_acotada_2_b(e1,L,3).
%    L = [a, a, b] ;
%    L = [a, b] ;
%    L = [b, a, b] ;
%    false.

% ----------------------------------------------------------------------
% Ejercicio 3. Se sabe que
% 1. Una banda está compuesta por tres músicos de distintos paises y que
%    tocan distintos instrumentos.
% 2. El pianista toca primero.
% 3. Juan toca el saxo y toca antes que el australiano.
% 4. Marcos es francés y toca antes que el violinista.
% 5. Hay un músico japonés.
% 6. Un músico se llama Saúl.
%
% Determinar el nombre, el país y el instrumento que toca cada uno de
% los músicos de la banda.
% ----------------------------------------------------------------------

% La relación solución_músicos(S) se verifica si S es una solución del
% problema de los músicos.
solución_músicos(S) :-
   % 1. Una banda está compuesta por tres músicos de distintos paises
   %    y que tocan distintos instrumentos.
   presolución(S),
   % 2. El pianista toca primero.
   instrumento(X,piano),
   primero(X,S),
   % 3. Juan toca el saxo y toca antes que el australiano.
   nombre(Y,juan),
   instrumento(Y,saxo),
   país(Z,australia),
   antes(Y,Z,S),
   % 4. Marcos es francés y toca antes que el violinista.
   nombre(Y1,marco),
   país(Y1,francia),
   instrumento(Z1,violín),
   antes(Y1,Z1,S),
   % 5. Hay un músico japonés.
   pertenece(U,S),
   país(U,japón),
   % 6. Un músico se llama Saúl.
   pertenece(V,S),
   nombre(V,saúl).

% La definición se basa en relación presolución(S) que se verifica si S
% es una presolución; i.e. S es un término de la forma
%    banda(músico(N1,P1,I1),
%          músico(N2,P2,I2),
%          músico(N3,P3,I3))
% donde músico(N,P,I) representa al músico de nombre N, país P y que
% toca el instrumento I.
presolución(banda(músico(_N1,_P1,_I1),
                  músico(_N2,_P2,_I2),
                  músico(_N3,_P3,_I3))).

% Además, se usan las siguientes relaciones auxiliares.

% nombre(X,N) se verifica si N es el el nombre del músico X.
nombre(músico(N,_,_),N).

% país(X,P) se verifica si P es el país del músico X.
país(músico(_,P,_),P).

% instrumento(X,I) se verifica si I es el instrumento que toca el músico
% X.
instrumento(músico(_,_,I),I).

% primero(X,B) se verifica si X es el primer músico de la banda B.
primero(X,banda(X,_,_)).

% pertenece(X,B) se verifica si X es un músico de la banda B.
pertenece(X,banda(X,_,_)).
pertenece(X,banda(_,X,_)).
pertenece(X,banda(_,_,X)).

% antes(X,Y,B) se verifica si X toca antes que Y en la banda B.
antes(X,Y,banda(X,Y,_)).
antes(X,Y,banda(X,_,Y)).
antes(X,Y,banda(_,X,Y)).

% La consulta para obtener la solución es
%    ?- solución_músicos(S).
%    S = [músico(marco, francia,   piano),
%         músico(juan,  japón,     saxo),
%         músico(saúl,  australia, violín)] ;
%    false.

% ----------------------------------------------------------------------
% Ejercicio 4.1. Supongamos que los cuadros del tablero de ajedrez los
% representamos por pares de números [X,Y] con X e Y entre 1 y 8.
%
% Definir la relación salta(+C1,?C2) que se verifica si el
% caballo puede pasar en un movimiento del cuadrado C1 al cuadrado
% C2. Por ejemplo,
%    ?- salta([1,1],S).
%    S = [3, 2] ;
%    S = [2, 3] ;
%    false.
% ----------------------------------------------------------------------

salta([X,Y],[X1,Y1]) :-
   dxy(Dx,Dy),
   X1 is X+Dx,
   correcto(X1),
   Y1 is Y+Dy,
   correcto(Y1).

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

% correcto(+X) se verifica si X está entre 1 y 8.
correcto(X) :-
   1 =< X,
   X =< 8.

% ----------------------------------------------------------------------
% Ejercicio 4.2. Definir la relación camino(L) que se verifique si L es
% una lista de cuadrados representando el camino recorrido por un
% caballo sobre  un tablero vacío. Por ejemplo,
%    ?- camino([[1,1],C]).
%    C = [3, 2] ;
%    C = [2, 3] ;
%    false.
% ----------------------------------------------------------------------

camino([_]).
camino([C1,C2|L]) :-
   salta(C1,C2),
   camino([C2|L]).

% ----------------------------------------------------------------------
% Ejercicio 4.3. Usando la relación camino, escribir una pregunta para
% determinar los caminos de longitud 4 por los que puede desplazarse un
% caballo desde cuadro [2,1] hasta el otro extremo del tablero (Y=8) de
% forma que en el segundo movimiento pase por el cuadro [5,4].
% ----------------------------------------------------------------------

% La consulta es
%    ?- camino([[2,1],C1,[5,4],C2,[X,8]]).
%    C1 = [4, 2],
%    C2 = [6, 6],
%    X = 7 ;
%    C1 = [4, 2],
%    C2 = [6, 6],
%    X = 5 ;
%    C1 = [4, 2],
%    C2 = [4, 6],
%    X = 5 ;
%    C1 = [4, 2],
%    C2 = [4, 6],
%    X = 3 ;
%    C1 = [3, 3],
%    C2 = [6, 6],
%    X = 7 ;
%    C1 = [3, 3],
%    C2 = [6, 6],
%    X = 5 ;
%    C1 = [3, 3],
%    C2 = [4, 6],
%    X = 5 ;
%    C1 = [3, 3],
%    C2 = [4, 6],
%    X = 3 ;

% ----------------------------------------------------------------------
% Ejercicio 4.4. Calcular el menor número de movimientos necesarios para
% desplazar el caballo del cuadro [1,1] al [2,2]. ¿Cuántos caminos de
% dicha longitud hay de [1,1] a [2,2]?
% ----------------------------------------------------------------------

% La consulta es
%    ?- camino([[1,1],_,[2,2]]).
%    false.
%
%    ?- camino([[1,1],_,_,[2,2]]).
%    false.
%
%    ?- camino([[1,1],_,_,_,[2,2]]).
%    true
%    ?- camino([[1,1],C2,C3,C4,[2,2]]).
%    C2 = [3, 2],
%    C3 = [5, 3],
%    C4 = [3, 4] ;
%    C2 = [3, 2],
%    C3 = [5, 3],
%    C4 = [4, 1] ;
%    C2 = [3, 2],
%    C3 = [5, 1],
%    C4 = [4, 3] ;
%    C2 = [3, 2],
%    C3 = [1, 3],
%    C4 = [3, 4] ;
%    C2 = [3, 2],
%    C3 = [2, 4],
%    C4 = [4, 3] ;
%    C2 = [2, 3],
%    C3 = [4, 2],
%    C4 = [3, 4] ;
%    C2 = [2, 3],
%    C3 = [3, 5],
%    C4 = [1, 4] ;
%    C2 = [2, 3],
%    C3 = [3, 5],
%    C4 = [4, 3] ;
%    C2 = [2, 3],
%    C3 = [3, 1],
%    C4 = [4, 3] ;
%    C2 = [2, 3],
%    C3 = [1, 5],
%    C4 = [3, 4] ;
%    false.

% ----------------------------------------------------------------------
% Ejercicio 5. Un árbol binario es vacío o consta de tres partes: la
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
% Definir la relación máximo(+T,-X) que se verifiQueca si X es el máximo
% de los nodos del árbol T. Por ejemplo,
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
   M1 is max(MI,MD),
   M is max(R,M1).

