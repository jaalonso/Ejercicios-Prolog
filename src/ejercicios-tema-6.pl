% ejercicios-tema-6.pl
% Ejercicios del tema 6 (Estilo y eficiencia en programación lógica).
% José A. Alonso Jiménez <https://jaalonso.github.io>
% Sevilla, 21-mayo-2022
% =============================================================================

% ------------------------------------------------------------------------------
% Ejercicio 1: Definir la relación lista_mayor(+L1,-L2) que se verifica si L2
% es una lista de la lista de listas L1 de máxima longitud. Por ejemplo,
%    ?- lista_mayor([[a,b,c],[d,e],[1,2,3]],L).
%    L = [a, b, c] ;
%    L = [1, 2, 3]
% ------------------------------------------------------------------------------

% 1ª solución
% ===========

lista_mayor_1(L1,L2) :-
   member(L2,L1),
   length(L2,N2),
   \+ (member(L,L1), length(L,N), N2 < N).

% 2ª solución
% ===========

lista_mayor_2(L1,L2) :-
   máxima_longitud(L1,N),
   length(L2,N),
   member(L2,L1).

% máxima_longitud(+L,-N) se verifica si N es la máxima longitud de la lista
% L. Por ejemplo,
%    ?- máxima_longitud([[a],[a,b,c],[b,a]],N).
%    N = 3
máxima_longitud([L],N) :-
   length(L,N).
máxima_longitud([L|R],N) :-
   length(L,N1),
   máxima_longitud(R,N2),
   N is max(N1,N2).

% 3ª solución
% ===========

lista_mayor_3([X|L1],L2):-
   length(X,N), !,
   lista_mayor_3_aux(L1,N-[X],L),
   member(L2,L).

lista_mayor_3_aux([],_-L,L).
lista_mayor_3_aux([Y|L1],N-Ac,L) :-
   length(Y,M),
   ( M < N ->
     lista_mayor_3_aux(L1,N-Ac,L)
   ; M = N ->
     lista_mayor_3_aux(L1,N-[Y|Ac],L)
   ; % M > N ->
     lista_mayor_3_aux(L1,M-[Y],L)
   ).

% Comparación de eficiencia
% =========================

% ejemplo_1(+N,-L) se verifica si L es la lista
% [[1],[1,2],[1,2,3],...,[1,2,...,N]]. Por  ejemplo,
%    ?- ejemplo_1(4,L).
%    L = [[1], [1, 2], [1, 2, 3], [1, 2, 3, 4]]
ejemplo_1(N,L) :-
   findall(L1,(between(1,N,M),numlist(1,M,L1)),L).

% La comparación es
%    ?- ejemplo_1(800,_L), time(lista_mayor_1(_L,_L1)).
%    % 1,287,998 inferences, 0.298 CPU in 0.298 seconds (100% CPU, 4327972 Lips)
%    true.
%
%    ?- ejemplo_1(800,_L), time(lista_mayor_2(_L,_L1)).
%    % 4,002 inferences, 0.007 CPU in 0.007 seconds (100% CPU, 541456 Lips)
%    true
%    ?- ejemplo_1(800,_L), time(lista_mayor_3(_L,_L1)).
%    % 4,000 inferences, 0.001 CPU in 0.001 seconds (100% CPU, 3062536 Lips)
%    true.

% ------------------------------------------------------------------------------
% Ejercicio 2: Un mapa puede representarse mediante la relación mapa(N,L)
% donde N es el nombre del mapa y L es la lista de los pares formados por cada
% una de las regiones del mapa y la lista de sus regiones vecinas. Por ejemplo,
% los mapas siguientes
%       +----------+----------+       +----+-----+-----+----+
%       |    a     |     b    |       | a  |  b  |  c  | d  |
%       +----+-----+-----+----+       +----+-----+-----+----+
%       |    |           |    |       | e  |           | f  |
%       | c  |     d     | e  |       +----+     k     +----+
%       |    |           |    |       | g  |           | h  |
%       +----+-----+-----+----+       +----+-----+-----+----+
%       |    f     |     g    |       |    i     |     j    |
%       +----------+----------+       +----------+----------+
% se pueden representar por
%    mapa(ejemplo_1,
%         [a-[b,c,d], b-[a,d,e], c-[a,d,f], d-[a,b,c,e,f,g],
%          e-[b,d,g], f-[c,d,g], g-[d,e,f]]).
%    mapa(ejemplo_2,
%         [a-[b,e,k],   b-[a,c,e,k], c-[b,d,f,k], d-[c,f,k], e-[a,b,g,k],
%          f-[c,d,h,k], g-[e,i,k],   h-[f,j,k],   i-[g,j,k], j-[i,h,k],
%          k-[a,b,c,d,e,f,g,h,i,j]]).
%
% Definir la relación coloración(+M,+LC,-S) que se verifica si S es una
% lista de pares formados por una región del mapa M y uno de los colores de la
% lista de colores LC tal que las regiones vecinas tengan colores distintos.
% Por ejemplo,
%    ?- coloración(ejemplo_1,[1,2,3],S).
%    S = [a-1, b-2, c-2, d-3, e-1, f-1, g-2]
%
% ¿Qué número de colores se necesitan para colorear el segundo mapa?. ¿De
% cuántas formas distintas puede colorearse con dicho número?.
% ------------------------------------------------------------------------------

mapa(ejemplo_1,
     [a-[b,c,d],
      b-[a,d,e],
      c-[a,d,f],
      d-[a,b,c,e,f,g],
      e-[b,d,g],
      f-[c,d,g],
      g-[d,e,f]]).

mapa(ejemplo_2,
     [a-[b,e,k],
      b-[a,c,e,k],
      c-[b,d,f,k],
      d-[c,f,k],
      e-[a,b,g,k],
      f-[c,d,h,k],
      g-[e,i,k],
      h-[f,j,k],
      i-[g,j,k],
      j-[i,h,k],
      k-[a,b,c,d,e,f,g,h,i,j]]).

mapa(andalucía,
     [huelva-[sevilla,cádiz],
      cádiz-[huelva,sevilla,málaga],
      sevilla-[huelva,cádiz,córdoba,málaga],
      córdoba-[sevilla,málaga,granada,jaén],
      jaén-[córdoba,granada,almería],
      granada-[córdoba,málaga,jaén,almería]]).

% 1ª solución
% ===========

coloración_1(M,LC,S) :-
   mapa(M,L),
   coloración_1_aux(L,LC,S).

coloración_1_aux([],_,[]).
coloración_1_aux([R-V|L],LC,[R-C|S]) :-
   member(C,LC),
   coloración_1_aux(L,LC,S),
   not((member(R1,V), member(R1-C,S))).

% 2ª solución
% ===========

coloración_2(M,LC,S) :-
   mapa(M,L),
   coloración_2_aux(L,LC,[],S).

coloración_2_aux([],_,S,S).
coloración_2_aux([R-V|L],LC,A,S) :-
   member(C,LC),
   not((member(R1,V), member(R1-C,A))),
   coloración_2_aux(L,LC,[R-C|A],S).

% Comparación de eficiencia
% =========================

% La comparación es
%    ?- set_prolog_flag(answer_write_options, [quoted(true), portray(true), max_depth(100)]).
%    true.
%
%    ?- time(coloración_1(ejemplo_2,[1,2,3,4],S)).
%    % 18,002,577 inferences, 1.297 CPU in 1.297 seconds (100% CPU, 13875597 Lips)
%    S = [a-1,b-2,c-1,d-2,e-3,f-3,g-1,h-1,i-2,j-3,k-4]
%    ?- time(coloración_2(ejemplo_2,[1,2,3,4],S)).
%    % 590 inferences, 0.000 CPU in 0.000 seconds (98% CPU, 1804977 Lips)
%    S = [k-4,j-3,i-2,h-1,g-1,f-3,e-3,d-2,c-1,b-2,a-1]

% Cuestiones
% ==========

% Para responder a las cuestiones, se hacen los siguientes cálculos
%    ?- findall(_S,coloración_2(ejemplo_2,[1,2,3],_S),_L), length(_L,N).
%    N = 0.
%
%    ?- findall(_S,coloración_2(ejemplo_2,[1,2,3,4],_S),_L), length(_L,N).
%    N = 1032.
% Por tanto, se necesitan 4 colores para colorear el segundo mapa y puede
% colorearse de 1032 formas con dicho número.

% ------------------------------------------------------------------------------
% Ejercicio 3.1.  Definir, usando un acumulador, la relación factorial(+X,-Y) que
% se verifica si Y es el factorial de X, Por ejemplo,
%    ?- factorial(3,X).
%    X = 6
% ------------------------------------------------------------------------------

factorial(X,Y) :-
   factorial_aux(X,1,Y).

factorial_aux(0,Y,Y).
factorial_aux(X,A,Y) :-
   X > 0,
   A1 is X*A,
   X1 is X-1,
   factorial_aux(X1,A1,Y).

% ------------------------------------------------------------------------------
% Ejercicio 3.2.  Definir, usando un acumulador, la relación longitud(L,N) que se
% verifica si N es la longitud de la lista L. Por ejemplo,
%    ?- longitud([a,b,a],X).
%    X = 3.
% ------------------------------------------------------------------------------

longitud(L,N) :-
   longitud_aux(L,0,N).

longitud_aux([],N,N).
longitud_aux([_|L],A,N) :-
   A1 is A+1,
   longitud_aux(L,A1,N).

% ------------------------------------------------------------------------------
% Ejercicio 4: Consideremos la siguiente versión del problema de las torres de
% Hanoi:
% + Existen tres postes que llamaremos A, B y C.
% + Hay N discos en el poste A ordenados por tamaño (el de arriba es el de
%   menor tamaño).
% + Los postes B y C están vacíos.
% + Sólo puede moverse un disco a la vez y todos los discos deben de
%   estar ensartados en algún poste.
% + Ningún disco puede situarse sobre otro de menor tamaño.
%
% Definir la relación hanoi(+N,+A,+B,+C,-L) que se verifica si L es la lista
% de movimientos para mover N discos desde el poste A al poste B usando el C
% como auxiliar. Por ejemplo,
%    ?- hanoi(4,a,b,c,L).
%    L = [a-c,a-b,c-b,a-c,b-a,b-c,a-c, a-b, c-b,c-a,b-a,c-b,a-c,a-b,c-b]
%    ?- hanoi(3,a,c,b,L).
%    L = [a-c,a-b,c-b,a-c,b-a,b-c,a-c]
%    ?- hanoi(3,c,b,a,L).
%    L = [c-b,c-a,b-a,c-b,a-c,a-b,c-b]
%
% Dar dos versiones (una sin memoria y otra con memoria) y comparar los
% resultados.
% ------------------------------------------------------------------------------

% 1ª solución (sin memoria)
% =========================

hanoi_1(1,A,B,_C,[A-B]).
hanoi_1(N,A,B,C,L) :-
   N > 1,
   N1 is N-1,
   hanoi_1(N1,A,C,B,L1),
   hanoi_1(N1,C,B,A,L2),
   append(L1,[A-B|L2],L).

% 2ª solución (con memoria)
% =========================

:- dynamic hanoi_2/5.

hanoi_2(1,A,B,_C,[A-B]).
hanoi_2(N,A,B,C,L) :-
   N > 1,
   N1 is N-1,
   lema(hanoi_2(N1,A,C,B,L1)),
   hanoi_2(N1,C,B,A,L2),
   append(L1,[A-B|L2],L).

lema(P) :-
   P,
   asserta((P :- !)).

% Comparación de eficiencia
% =========================

% La comparación es
%    ?- time(hanoi_1(20,a,b,c,_L)).
%    % 11,534,332 inferences, 2.083 CPU in 2.083 seconds (100% CPU, 5537864 Lips)
%    true
%    ?- time(hanoi_2(20,a,b,c,_L)).
%    % 1,966,421 inferences, 0.387 CPU in 0.387 seconds (100% CPU, 5086393 Lips)

% ------------------------------------------------------------------------------
% Ejercicio 5. El problema de la bandera tricolor consiste en lo siguiente:
% Dada un lista de objetos L1, que pueden ser rojos, amarillos o morados, se
% pide devolver una lista L2 que contiene los elementos de L1, primero los
% rojos, luego los amarillos y por último los morados.
%
% Definir la relación bandera_tricolor(+L1,-L2) que se verifica si L2 es la
% solución del problema de la bandera tricolor correspondiente a la lista
% L1. Por ejemplo,
%    ?- bandera_tricolor([m,m,a,a,r,r],L).
%    L = [r, r, a, a, m, m]
%
% Se dispone de los hechos
%    rojo(r).
%    amarillo(a).
%    morado(m).
%
% Dar distintas definiciones y comparar su eficiencia.
% ------------------------------------------------------------------------------

rojo(r).
amarillo(a).
morado(m).

% 1ª solución
% ===========

bandera_tricolor_1(L1,L2) :-
   permutation(L1,L2),
   rojo_amarillo_morado(L2).

rojo_amarillo_morado(L) :-
   append(Rojo,Amarillo_y_morado,L),
   lista(rojo,Rojo),
   append(Amarillo,Morado,Amarillo_y_morado),
   lista(amarillo,Amarillo),
   lista(morado,Morado).

lista(_,[]).
lista(C,[X|L]) :-
   apply(C,[X]),
   lista(C,L).

% 2ª solución (con separación mediante bagof)
% ===========================================

bandera_tricolor_2(L1,L2) :-
   bagof(X,(member(X,L1),rojo(X)),Rojos),
   bagof(X,(member(X,L1),amarillo(X)),Amarillos),
   bagof(X,(member(X,L1),morado(X)),Morados),
   append(Amarillos,Morados,AmarillosMorados),
   append(Rojos,AmarillosMorados,L2).

% 3ª solución (mediante selección y negación)
% ===========================================

bandera_tricolor_3(L1,L2) :-
   bandera_tricolor_3_aux(L1,[],L2).

bandera_tricolor_3_aux(L1,L2,L3) :-
   select(X,L1,RL1), morado(X),
   bandera_tricolor_3_aux(RL1,[X|L2],L3).
bandera_tricolor_3_aux(L1,L2,L3) :-
   not((select(X,L1,_),morado(X))),
   select(Y,L1,RL1), amarillo(Y),
   bandera_tricolor_3_aux(RL1,[Y|L2],L3).
bandera_tricolor_3_aux(L1,L2,L3) :-
   not((select(X,L1,_), morado(X))),
   not((select(Y,L1,_), amarillo(Y))),
   append(L1,L2,L3).

% 4ª solución (mediante selección y corte)
% ========================================

bandera_tricolor_4(L1,L2) :-
   bandera_tricolor_4_aux(L1,[],L2).

bandera_tricolor_4_aux(L1,L2,L3) :-
   select(X,L1,RL1), morado(X), !,
   bandera_tricolor_4_aux(RL1,[X|L2],L3).
bandera_tricolor_4_aux(L1,L2,L3) :-
   % not((select(X,L1,_), morado(X))),
   select(Y,L1,RL1), amarillo(Y), !,
   bandera_tricolor_4_aux(RL1,[Y|L2],L3).
bandera_tricolor_4_aux(L1,L2,L3) :-
   % not((select(X,L1,_), morado(X))),
   % not((select(Y,L1,_), amarillo(Y))),
   append(L1,L2,L3).

% 5ª Solución (mediante separación)
% =================================

bandera_tricolor_5(L1,L2) :-
   separa(L1,R,A,M),
   une(R,A,M,L2).

separa([],[],[],[]).
separa([X|L],[X|R],A,M) :-
   rojo(X), !,
   separa(L,R,A,M).
separa([X|L],R,[X|A],M) :-
   amarillo(X), !,
   separa(L,R,A,M).
separa([X|L],R,A,[X|M]) :-
   % morado(X),
   separa(L,R,A,M).

une(R,A,M,L) :-
   append(A,M,AM),
   append(R,AM,L).

% 6ª solución
% ===========

bandera_tricolor_6(L1,L2) :-
   aux_1(L1,L2,X,X,Y,Y,[]).

aux_1([],R,R,B,B,A,A).
aux_1([X|Resto],R0,R,B0,B,A0,A) :-
   color(X,Color),
   aux_2(Color,R0,R,B0,B,A0,A,X,Resto).

aux_2(rojo,[X|R1],R,B0,B,A0,A,X,Resto) :-
   aux_1(Resto,R1,R,B0,B,A0,A).
aux_2(amarillo,R0,R,[X|B1],B,A0,A,X,Resto) :-
   aux_1(Resto,R0,R,B1,B,A0,A).
aux_2(morado,R0,R,B0,B,[X|A1],A,X,Resto) :-
   aux_1(Resto,R0,R,B0,B,A1,A).

color(X,Color) :-
   member(Color,[morado,rojo,amarillo]),
   Atom =.. [Color,X],
   Atom.

% 7ª solución (con include)
% =========================

bandera_tricolor_7(L1,L2) :-
   include(rojo,L1,Rojos),
   include(amarillo,L1,Amarillos),
   include(morado,L1,Morados),
   append(Amarillos,Morados,AmarillosMorados),
   append(Rojos,AmarillosMorados,L2).

% Comparación de eficiencia
% =========================

% La comparación es
%    ?- time(bandera_tricolor_1([m,a,r,m,a,r,m,a,r],L)).
%    % 16,262,993 inferences, 1.442 CPU in 1.442 seconds (100% CPU, 11281493 Lips)
%    L = [r, r, r, a, a, a, m, m, m]
%
%    ?- time(bandera_tricolor_2([m,a,r,m,a,r,m,a,r],L)).
%    % 110 inferences, 0.000 CPU in 0.000 seconds (95% CPU, 1054397 Lips)
%    L = [r, r, r, a, a, a, m, m, m].
%
%    ?- time(bandera_tricolor_3([m,a,r,m,a,r,m,a,r],L)).
%    % 159 inferences, 0.000 CPU in 0.000 seconds (98% CPU, 1468144 Lips)
%    L = [r, r, r, a, a, a, m, m, m]
%
%    ?- time(bandera_tricolor_4([m,a,r,m,a,r,m,a,r],L)).
%    % 94 inferences, 0.000 CPU in 0.000 seconds (90% CPU, 1563228 Lips)
%    L = [r, r, r, a, a, a, m, m, m].
%
%    ?- time(bandera_tricolor_5([m,a,r,m,a,r,m,a,r],L)).
%    % 34 inferences, 0.000 CPU in 0.000 seconds (91% CPU, 603875 Lips)
%    L = [r, r, r, a, a, a, m, m, m].
%
%    ?- time(bandera_tricolor_6([m,a,r,m,a,r,m,a,r],L)).
%    % 90 inferences, 0.000 CPU in 0.000 seconds (93% CPU, 1261069 Lips)
%    L = [r, r, r, a, a, a, m, m, m]
%
%    ?- time(bandera_tricolor_7([m,a,r,m,a,r,m,a,r],L)).
%    % 86 inferences, 0.000 CPU in 0.000 seconds (95% CPU, 847357 Lips)
%    L = [r, r, r, a, a, a, m, m, m].

% ------------------------------------------------------------------------------
% Ejercicio 6. Se dice que L es una sucesión de Lanford si L es una lista de
% longitud 27 en la cual aparecen 3 veces cada uno de los dígitos del 1 al 9 y
% que además cumple la propiedad de que entre dos 1 siempre hay un dígito,
% entre dos 2 hay  dos dígitos, entre dos 3 hay tres dígitos, etc.
%
% Definir la relación lanford(?L) que se verifique si L es una longitud de
% Lanford. Por ejemplo,
%    ?- lanford(L).
%    L = [1,9,1,2,1,8,2,4,6,2,7,9,4,5,8,6,3,4,7,5,3,9,6,8,3,5,7] ;
% ------------------------------------------------------------------------------

lanford(L):-
   length(L,27),
   sublista([1,_,1,_,1],L),
   sublista([2,_,_,2,_,_,2],L),
   sublista([3,_,_,_,3,_,_,_,3],L),
   sublista([4,_,_,_,_,4,_,_,_,_,4],L),
   sublista([5,_,_,_,_,_,5,_,_,_,_,_,5],L),
   sublista([6,_,_,_,_,_,_,6,_,_,_,_,_,_,6],L),
   sublista([7,_,_,_,_,_,_,_,7,_,_,_,_,_,_,_,7],L),
   sublista([8,_,_,_,_,_,_,_,_,8,_,_,_,_,_,_,_,_,8],L),
   sublista([9,_,_,_,_,_,_,_,_,_,9,_,_,_,_,_,_,_,_,_,9],L).

sublista(L1,L2):-
   append(_,L3,L2),
   append(L1,_,L3).

% ----------------------------------------------------------------------
% Ejercicio 7.1. En cierta ocasión, el matemático Ramanujan estaba en
% un hospital en Inglaterra y su amigo Hardy fue a visitarlo. Hardy
% comentó que había llegado al hospital en un taxi de matrícula N y
% esperaba que éste no fuese un mal presagio, ya que N era un número
% poco interesante. Ramanujan no estuvo de acuerdo ya que
% inmediatamente dijo que N tiene una propiedad muy especial: N es el
% menor entero positivo que puede descomponerse de dos maneras
% distintas como suma de dos cubos.
%
% El objetivo de este ejercicio es averiguar la matrícula del taxi que
% llevó a Hardy a visitar a Ramanujan.
%
% Definir la relación es_cubo(+N) que se verifique si N es el cubo de un
% entero. Por ejemplo,
%    ?- es_cubo_1(1000).
%    Yes
%    ?- es_cubo_1(1001).
%    No
% ----------------------------------------------------------------------

% 1ª solución
% ===========

es_cubo_1(N) :-  
   between(1,N,X),
   N is X*X*X.    

% 2ª solución
% ===========

es_cubo_2(N) :-
   Cota is round(N^(1/3)),   
   between(1,Cota,X),        
   N is X*X*X.               

% 3ª solución
% ===========

es_cubo_3(N) :-
   N =:= round(N ** (1/3)) ** 3.

% Comparación de eficiencia:
%    ?- time(es_cubo_1(1000001)).
%    % 2,000,005 inferences, 0.153 CPU in 0.153 seconds (100% CPU, 13041583 Lips)
%    false.
%    
%    ?- time(es_cubo_2(1000001)).
%    % 202 inferences, 0.000 CPU in 0.000 seconds (97% CPU, 1422045 Lips)
%    false.
%    
%    ?- time(es_cubo_3(1000001)).
%    % 2 inferences, 0.000 CPU in 0.000 seconds (87% CPU, 51219 Lips)
%    false.

% En lo que sigue adoptaremos la tercera como definicón de es_cubo.
es_cubo(N) :-
   es_cubo_3(N).

% ----------------------------------------------------------------------
% Ejercicio 7.2. Definir la relación descompone(+N,-X,-Y) que se
% verifique si X e Y son dos cubos cuya suma es N y, además, X es menor
% o igual que Y. Por ejemplo,
%    ?- descompone(1008,X,Y).
%    X = 8
%    Y = 1000 ;
%    No
% ----------------------------------------------------------------------

% 1ª solución
% ===========

descompone_1(N,X,Y) :- 
    between(1,N,X),         
    between(1,N,Y),         
    es_cubo(X),            
    es_cubo(Y),            
    X =< Y,            
    N is X+Y.          

% 2ª solución
% ===========

descompone_2(N,X,Y) :- 
    between(1,N,X),    
    es_cubo(X),        
    Y is N - X,        
    X =< Y,            
    es_cubo(Y).        

% 3ª solución
% ===========

descompone_3(N,X,Y) :-  
    Cota is round((N/2)^(1/3)), 
    between(1,Cota,M),          
    X is M*M*M,             
    Y is N-X,          
    X =< Y,            
    es_cubo(Y).            

% Comparación de eficiencia
%    ?- time(descompone_1(1707,X,Y)).
%    % 11,713,622 inferences, 1.061 CPU in 1.061 seconds (100% CPU, 11036044 Lips)
%    false.
%    
%    ?- time(descompone_2(1707,X,Y)).
%    % 6,878 inferences, 0.001 CPU in 0.001 seconds (100% CPU, 6876583 Lips)
%    false.
%    
%    ?- time(descompone_3(1707,X,Y)).
%    % 65 inferences, 0.000 CPU in 0.000 seconds (94% CPU, 907479 Lips)
%    false.

% En lo que sigue adoptaremos la tercera como definición de descompone.
descompone(N,X,Y) :-
   descompone_3(N,X,Y).

% ----------------------------------------------------------------------
% Ejercicio 7.3. Definir la relación ramanujan(+N) que se verifique si N
% puede descomponerse en suma de dos cubos exactamente de dos maneras
% distintas.
% ----------------------------------------------------------------------

ramanujan(N) :-
    setof(par(X,Y),descompone(N,X,Y),[_,_]).

% ----------------------------------------------------------------------
% Ejercicio 7.4. Definir la relación hardy(-N) que se verifique si N es
% el menor entero positivo que satisface el predicado ramanujan
% anterior. ¿Cuál es la la matrícula del taxi que llevó a Hardy a
% visitar a Ramanujan?
% ----------------------------------------------------------------------

hardy(N) :-         
    hardy_aux(N,1). 
hardy_aux(N,N) :-   
    ramanujan(N),   
    !.              
hardy_aux(N,M) :-   
    M1 is M+1,     
    hardy_aux(N,M1).

% La matrícula del taxi que llevó a  Hardy a visitar a Ramanujan se calcula
% mediante la siguiente consulta
%    ?- hardy(N).
%    N = 1729
% Por tanto, la matrícula del taxi es 1729.
