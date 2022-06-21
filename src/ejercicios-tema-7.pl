% ejercicios-tema-7.pl
% Ejercicios del tema 7 (Aplicaciones: Grafos, reinas y mapas).
% José A. Alonso Jiménez <https://jaalonso.github.io>
% Sevilla, 24-mayo-2022
% ======================================================================

% Nota. Usaremos las relaciones de camino.pl
:- [camino].

% También usaremos los siguientes ejemplos de grafo
grafo(g1,[a-b,b-c,b-d,c-d]).
grafo(g2,[a-b,c-d]).
grafo(g3,[a-b,b-c,b-d]).
grafo(g4,[a-b,b-c]).

% ----------------------------------------------------------------------
% Ejercicio 1. Definir la relación conectado(+G) que se verifica si el
% grafo G está conectado; es decir, existe un camino entre cada par de
% vértices distintos. Por ejemplo,
%    ?- conectado(g1).
%    true.
%
%    ?- conectado(g2).
%    false.
%
%    ?- conectado(g3).
%    true.
% ----------------------------------------------------------------------

conectado(G) :-
   not((nodo(G,X), nodo(G,Y), not(camino(G,X,Y,_)))).

% ----------------------------------------------------------------------
% Ejercicio 2. Definir la relación tiene_ciclos(+G) que se verifica si
% en el grafo G hay ciclos. Por ejemplo,
%    ?- tiene_ciclos(g1).
%    true
%
%    ?- tiene_ciclos(g2).
%    false.
%
%    ?- tiene_ciclos(g3).
%    false.
% ----------------------------------------------------------------------

tiene_ciclos(G) :-
   adyacente(G,X,Y),
   camino(G,X,Y,[X,_,_|_]).

% ----------------------------------------------------------------------
% Ejercicio 3. Definir la relación es_árbol(+G) que se verifica si G es
% un árbol; es decir, G es un grafo conectado sin ciclos. Por ejemplo,
%    ?- es_árbol(g1).
%    false.
%
%    ?- es_árbol(g2).
%    false.
%
%    ?- es_árbol(g3).
%    true.
% ----------------------------------------------------------------------

es_árbol(G) :-
   conectado(G),
   not(tiene_ciclos(G)).

% ----------------------------------------------------------------------
% Ejercicio 4. Definir la relación recubre(+G1,+G2) que se verifica si
% el grafo G1 recubre el grafo G2 (es decir, todos los nodos del grafo
% G2 son nodos del grafo G1). Por ejemplo,
%    ?- recubre(g3,g4).
%    true.
%
%    ?- recubre(g4,g3).
%    false.
% ----------------------------------------------------------------------

recubre(G1,G2) :-
   not((nodo(G2,X), not(nodo(G1,X)))).

% ----------------------------------------------------------------------
% Ejercicio 4. Definir la relación árbol_de_expansión(+G,?A) que se
% verifica si A es un árbol de expansión de G; es decir, A es un
% subgrafo de G que es un árbol y recubre a G. Por ejemplo,
%    ?- árbol_de_expansión(g1,A).
%    A = grafo(ae2, [a-b, b-c, b-d]) ;
%    A = grafo(ae3, [a-b, b-c, c-d]) ;
%    A = grafo(ae5, [a-b, b-d, c-d]) ;
%    false.
% ----------------------------------------------------------------------

:- dynamic grafo/2.

árbol_de_expansión(G,A) :-
   reset_gensym(ae),
   árbol_de_expansión_aux(G,A).

árbol_de_expansión_aux(G,A) :-
   arcos(G,L1),
   subconjunto(L2,L1),
   gensym(ae,E),
   A =.. [grafo,E,L2],
   asserta(A),
   es_árbol(E),
   recubre(E,G).

% subconjunto(?L1,+L2) se verifica si L1 es un subconjunto de L2. Por
% ejemplo,
%    ?- subconjunto(L,[a,b,c]).
%    L = [a, b, c] ;
%    L = [a, b] ;
%    L = [a, c] ;
%    L = [a] ;
%    L = [b, c] ;
%    L = [b] ;
%    L = [c] ;
%    L = [].
subconjunto([],[]).
subconjunto([X|L1],[X|L2]) :-
   subconjunto(L1,L2).
subconjunto(L1,[_|L2]) :-
   subconjunto(L1,L2).

% ----------------------------------------------------------------------
% Ejercicio 5.1. Mediante la relación lista_de_clase(C,A,L) se
% representa la información de los alumnos según los cursos y
% asignaturas, de forma que C es el curso, A es la asignatura y L es la
% lista de los alumnos de dicha asignatura. A lo largo del ejercicio
% vamos a usar como ejemplo la siguiente información.
%    lista_de_clase(c1,asig1,[a1,a5]).
%    lista_de_clase(c1,asig2,[a1,a2,a3]).
%    lista_de_clase(c1,asig3,[a1,a3]).
%    lista_de_clase(c1,asig4,[a2,a4,a6,a8]).
%    lista_de_clase(c1,asig5,[a2,a4,a5]).
%    lista_de_clase(c1,asig6,[a6,a7]).
%    lista_de_clase(c1,asig7,[a3,a7]).
%    lista_de_clase(c1,asig8,[a6,a7,a8]).
%    lista_de_clase(c2,asig9,[a9,a11]).
%    lista_de_clase(c2,asig10,[a10,a12]).
%    lista_de_clase(c2,asig11,[a10,a11]).
%    lista_de_clase(c2,asig12,[a9,a12]).
%
% Definir la relación asignaturas(+C,-L) que se verifique si L es la
% lista de asignaturas del curso C. Por ejemplo,
%    ?- asignaturas(c1,L).
%    L = [asig1,asig2,asig3,asig4,asig5,asig6,asig7,asig8].
%    
%    ?- asignaturas(c2,L).
%    L = [asig9,asig10,asig11,asig12].
% ----------------------------------------------------------------------

lista_de_clase(c1,asig1,[a1,a5]).
lista_de_clase(c1,asig2,[a1,a2,a3]).
lista_de_clase(c1,asig3,[a1,a3]).
lista_de_clase(c1,asig4,[a2,a4,a6,a8]).
lista_de_clase(c1,asig5,[a2,a4,a5]).
lista_de_clase(c1,asig6,[a6,a7]).
lista_de_clase(c1,asig7,[a3,a7]).
lista_de_clase(c1,asig8,[a6,a7,a8]).
lista_de_clase(c2,asig9,[a9,a11]).
lista_de_clase(c2,asig10,[a10,a12]).
lista_de_clase(c2,asig11,[a10,a11]).
lista_de_clase(c2,asig12,[a9,a12]).

asignaturas(C,L):-
   findall(X,lista_de_clase(C,X,_),L).

% ----------------------------------------------------------------------
% Ejercicio 5.2. Definir la relación grupo_incompatible(+L) que se
% verifique si la lista de asignaturas L es incompatible (es decir,
% algún alumno está en las listas de clase de más de una asignatura de
% la lista L). Por ejemplo,
%    ?- grupo_incompatible([asig1,asig2]).
%    true 
%    ?- grupo_incompatible([asig1,asig4,asig7]).
%    false.
% ----------------------------------------------------------------------

grupo_incompatible(L):-
   select(X,L,R),
   member(Y,R),
   lista_de_clase(_,X,LX),
   lista_de_clase(_,Y,LY),
   member(A,LX),
   member(A,LY).

% ----------------------------------------------------------------------
% Ejercicio 5.3. Definir la relación lista_incompatible(+L) que
% verifique si la lista de grupos de asignaturas L es incompatible (es
% decir, contiene algún grupo incompatible de asignaturas). Por ejemplo,
%    ?- lista_incompatible([[asig9, asig12], [asig11, asig10]]).
%    true 
%    ?- lista_incompatible([[asig11, asig12], [asig9, asig10]]).
%    false.
% ----------------------------------------------------------------------

lista_incompatible(P) :-
   member(G,P),
   grupo_incompatible(G).

% ----------------------------------------------------------------------
% Ejercicio 5.4. Definir la relación extensión(+L1,+X,-L2) que se
% verifique si L2 es la lista obtenida añadiendo X como primer elemento
% de un elemento de L1 o L2 es la lista obtenida añadiendo [X] a L1. Por
% ejemplo,
%    ?- extensión([[a], [b, c]],d,E).
%    E = [[d,a],[b,c]] ;
%    E = [[a],[d,b,c]] ;
%    E = [[a],[b,c],[d]].
% ----------------------------------------------------------------------

extensión([],X,[[X]]).
extensión([L|L1],X,[[X|L]|L1]).
extensión([L|L1],X,[L|L2]) :-
   extensión(L1,X,L2).

% ----------------------------------------------------------------------
% Ejercicio 5.5. Definir la relación partición(+L,-P) que se verifique
% si P es una partición de la lista L (es decir, un conjunto obtenido
% distribuyendo los elementos de L en conjuntos no vacíos y sin
% elementos comunes). Por ejemplo,
%    ?- partición([a,b,c],P).
%    P = [[a,b,c]] ;
%    P = [[b,c],[a]] ;
%    P = [[a,c],[b]] ;
%    P = [[c],[a,b]] ;
%    P = [[c],[b],[a]].
% ----------------------------------------------------------------------

partición([],[]).
partición([X|L1],L2) :-
   partición(L1,L3),
   extensión(L3,X,L2).
 
% ----------------------------------------------------------------------
% Ejercicio 5.6. Definir la relación agrupación_compatible_de_asignaturas(+C,-P)
% que se verifique si P es una partición compatible de las asignaturas
% del curso C. Por ejemplo,
%    ?- agrupación_compatible_de_asignaturas(c2,P).
%    P = [[asig11,asig12],[asig9,asig10]] ;
%    P = [[asig11,asig12],[asig10],[asig9]] ;
%    P = [[asig12],[asig11],[asig9,asig10]] 
% ----------------------------------------------------------------------

agrupación_compatible_de_asignaturas(C,P) :-
   asignaturas(C,L),
   partición(L,P),
   not(lista_incompatible(P)).
   
% ----------------------------------------------------------------------
% Ejercicio 5.7. Definir la relación agrupación_compatible_minimal(+C,-P)
% que se verifique si P es una partición compatible de las asignaturas
% del curso C con el menor número posible de grupos de asignaturas. Por
% ejemplo,
%    ?- agrupación_compatible_minimal(c2,P).
%    P = [[asig11,asig12],[asig9,asig10]] ;
%    false.
% ----------------------------------------------------------------------

agrupación_compatible_minimal(C,P) :-
   agrupación_compatible_de_asignaturas(C,P),
   length(P,N),
   not((agrupación_compatible_de_asignaturas(C,P1),length(P1,M),M < N)).

% ----------------------------------------------------------------------
% Ejercicio 5.8. Calcular las agrupaciones compatibles minimales del
% curso c1.
% ----------------------------------------------------------------------

% Solución
%    ?- agrupación_compatible_minimal(c1,P).
%    P = [[asig3,asig5,asig8],[asig1,asig4,asig7],[asig2,asig6]] ;
%    P = [[asig2,asig8],[asig1,asig4,asig7],[asig3,asig5,asig6]] ;
%    false.

% ----------------------------------------------------------------------
% Ejercicio 6.1. El objetivo de los siguientes ejercicios es la
% simulación de una calculadora básica. Para ello consideraremos que en
% cada momento la calculadora se encuentra en un determinado estado
% caracterizado por una lista con cuatro elementos [UCE,UTA,UOA,VIM]
% donde
% + UCE es el último cálculo efectuado,
% + UTA es la última tecla activada, 
% + UOA es el último operador activado y
% + VIM es el valor impreso.
%
% El estado inicial es [0,=,=,0] y está definido por
%    estado_inicial([0,=,=,0]).
%
% Las acciones posibles son pulsar un dígito, una operación aritmética o
% la de resultado y están definidas por
%    acción(X) :- es_dígito(X).
%    acción(X) :- es_operación(X).
%    acción(X) :- es_resultado(X).
%    
%    es_dígito(0).
%    es_dígito(1).
%    es_dígito(2).
%    es_dígito(3).
%    es_dígito(4).
%    es_dígito(5).
%    es_dígito(6).
%    es_dígito(7).
%    es_dígito(8).
%    es_dígito(9).  
%    
%    es_operación(+).
%    es_operación(-).
%    es_operación(*).
%    es_operación(/).
%    
%    es_resultado(=).
%
% En la siguiente tabla se muestran los estados de la calculadora
% correspondientes a las acciones indicadas en la última columna
%   |----------------+-------|
%   | estado         | tecla |
%   |----------------+-------|
%   | ( 0, =, =,  0) |  3    |
%   | ( 0, 3, =,  3) |  +    |
%   | ( 3, +, +,  3) |  2    |
%   | ( 3, 2, +,  2) |  1    |
%   | ( 3, 1, +, 21) |  *    |
%   | (24, *, *, 24) |  2    |
%   | (24, 2, *,  2) |  =    |
%   | (48, =, =, 48) |       |
%   |----------------+-------|
% Es decir, si se parte del estado inicial y se realizan las acciones
%    3 + 2 1 * 2 =
% se obtiene como resultado el número 48.
% 
% Definir la relación transición(+E1,+X,?E2) que se verifique si E2 es
% el estado obtenido aplicando la acción X al estado E1; es decir, si E1
% es [UCE,UTA,UOA,VIM], entonces
% + Si X es un dígito, entonces
%   + si UTA es un dígito, E2 es [UCE,X,UOA,10*VIM+X];
%   + en otro caso, E2 es [UCE,X,UOA,X].
% + Si X no es un dígito, entonces
%   + si UOA es una operación, E2 es [UOA(UCE,VIM),X,X,UOA(UCE,VIM)] 
%   + en otro caso, E2 es [VIM,X,X,VIM].
% Por ejemplo,
%   ?- estado_inicial(E1),
%      transición(E1,3,E2),
%      transición(E2,+,E3),
%      transición(E3,2,E4),
%      transición(E4,1,E5),
%      transición(E5,*,E6),
%      transición(E6,2,E7),
%      transición(E7,=,E8).
%   E1 = [0, =, =, 0]
%   E2 = [0, 3, =, 3]
%   E3 = [3, +, +, 3]
%   E4 = [3, 2, +, 2]
%   E5 = [3, 1, +, 21]
%   E6 = [24, *, *, 24]
%   E7 = [24, 2, *, 2]
%   E8 = [48, =, =, 48].
% ----------------------------------------------------------------------

estado_inicial([0,=,=,0]).

acción(X) :- es_dígito(X).
acción(X) :- es_operación(X).
acción(X) :- es_resultado(X).

es_dígito(0).
es_dígito(1).
es_dígito(2).
es_dígito(3).
es_dígito(4).
es_dígito(5).
es_dígito(6).
es_dígito(7).
es_dígito(8).
es_dígito(9).  

es_operación(+).
es_operación(-).
es_operación(*).
es_operación(/).

es_resultado(=).

% 1ª solución
% ===========

transición_1([UCE,UTA,UOA,VIM],X,E) :-
   ( es_dígito(X) ->
     ( es_dígito(UTA) ->
       Y is 10*VIM+X,
       E = [UCE,X,UOA,Y]
     ; % \+ es_dígito(UTA) ->
       E = [UCE,X,UOA,X] )
   ; % \+ es_dígito(X) ->
     ( es_operación(UOA) ->
       T =.. [UOA,UCE,VIM],
       Y is T,
       E = [Y,X,X,Y]
     ; % \+ es_operación(UOA) ->
       E = [VIM,X,X,VIM] )).

% 2ª solución
% ===========

transición_2([UCE,UTA,UOA,VIM],X,E) :-
   ( es_dígito(X), es_dígito(UTA) ->
     Y is 10*VIM+X,
     E = [UCE,X,UOA,Y]
   ; es_dígito(X), \+ es_dígito(UTA) ->
     E = [UCE,X,UOA,X]
   ; \+ es_dígito(X), es_operación(UOA) ->
     T =.. [UOA,UCE,VIM],
     Y is T,
     E = [Y,X,X,Y]
   ; \+ es_dígito(X), es_resultado(UOA) ->
     E = [VIM,X,X,VIM] ).

% 3ª solución
% ===========

transición_3([UCE,UTA,UOA,VIM],X,[UCE,X,UOA,Y]) :-
   es_dígito(X),
   es_dígito(UTA),
   Y is 10*VIM+X.
transición_3([UCE,UTA,UOA,_VIM],X,[UCE,X,UOA,X]) :-
   es_dígito(X),
   \+ es_dígito(UTA).
transición_3([UCE,_UTA,UOA,VIM],X,[Y,X,X,Y]) :-
   \+ es_dígito(X),
   es_operación(UOA),
   T =.. [UOA,UCE,VIM],
   Y is T.
transición_3([_UCE,_UTA,=,VIM],X,[VIM,X,X,VIM]) :-
   \+ es_dígito(X).

% En lo que sige usaremos la primera.
transición(E1,A,E2) :-
   transición_1(E1,A,E2).

% ----------------------------------------------------------------------
% Ejercicio 6.2. Definir la relación transiciones(+E1,+L,?E2) que se
% verifique si E2 es el estado obtenido aplicando las acciones de la
% lista L al estado E1. Por ejemplo,
%    ?- estado_inicial(_E1), transiciones(_E1,[3,+,2,1,*,2,=],E2).
%    E2 = [48, =, =, 48] 
% ----------------------------------------------------------------------

transiciones(E,[],E).
transiciones(E1,[X|L],E3) :-
   transición(E1,X,E2),
   transiciones(E2,L,E3).

% ----------------------------------------------------------------------
% Ejercicio 6.3. Definir la relación acciones(?L) que se verifique si L
% es una lista cuyos elementos son acciones. Por ejemplo,
%    ?- acciones([2,+,3,7]).
%    true 
%    ?- acciones([2,+,37]).
%    false.
% ----------------------------------------------------------------------

acciones([]).
acciones([X|L]) :-
   acción(X),
   acciones(L).

% ----------------------------------------------------------------------
% Ejercicio 6.4. Calcular el número de posibles listas de acciones de
% longitud 3. 
% ----------------------------------------------------------------------


% Solución
%    ?- findall(_L,(length(_L,3), acciones(_L)),_LAL3), length(_LAL3,N).
%    N = 3375.

% ----------------------------------------------------------------------
% Ejercicio 6.5. Definir la relación empieza_por_dígito(?L) que se
% verifique si el primer elemento de la lista L es un dígito.
% ----------------------------------------------------------------------

empieza_por_dígito([X|_L]) :-
   es_dígito(X).

% ----------------------------------------------------------------------
% Ejercicio 6.6. Definir la relación tiene_operaciones_consecutivas(?L)
% que se verifique si la lista L contiene dos operaciones consecutivas.
% ----------------------------------------------------------------------

tiene_operaciones_consecutivas(L) :-
   append(_A,[X,Y|_B],L),
   es_operación(X),
   es_operación(Y).

% ----------------------------------------------------------------------
% Ejercicio 6.7. Definir la relación tiene_resultado_intermedio(?L) que
% se verifique si la lista L contiene el símbolo = en una posición que
% no es la última.
% ----------------------------------------------------------------------

tiene_resultado_intermedio(L) :-
   append(_A,[=,_Y|_B],L).

% ----------------------------------------------------------------------
% Ejercicio 6.8. Definir la relación divide_por_cero(?L) que se
% verifique si en la lista L aparecen de manera consecutiva el símbolo /
% y un cero.
% ----------------------------------------------------------------------

divide_por_cero(L) :-
   append(_A,[/,0|_B],L).

% ----------------------------------------------------------------------
% Ejercicio 6.9. Definir la relación termina_en_dígito_y_resultado(?L)
% que se verifique si en la lista L los últimos elementos son un dígito
% y el símbolo =.
% ----------------------------------------------------------------------

termina_en_dígito_y_resultado(L) :-
   reverse(L,[=,X|_]),
   es_dígito(X).

% ----------------------------------------------------------------------
% Ejercicio 6.10. Definir la relación acciones_válidas(L) que se
% verifique si L es una lista de acciones válidas.
% ----------------------------------------------------------------------

acciones_válidas(L) :-
   acciones(L),
   empieza_por_dígito(L),
   not(tiene_operaciones_consecutivas(L)),
   not(tiene_resultado_intermedio(L)),
   not(divide_por_cero(L)),
   termina_en_dígito_y_resultado(L).

% ----------------------------------------------------------------------
% Ejercicio 6.11. Calcular el número de posibles listas de acciones
% válidas de longitud 3.
% ----------------------------------------------------------------------

% Solución
%    ?- findall(_L,(length(_L,3), acciones_válidas(_L)),_LAL3), length(_LAL3,N).
%    N = 100.

% ----------------------------------------------------------------------
% Ejrcicio 6.12. Definir la relación cálculo(+N,+M,-L) que se verifique
% si L es una lista de M acciones válidas que aplicadas al estado
% inicial da como resultado el número N. Por ejemplo,
%    ?- cálculo(5,2,L).
%    L = [5,=] ;
%    false.
%    
%    ?- cálculo(5,3,L).
%    L = [0,5,=] ;
%    false.
%    
%    ?- cálculo(5,4,L).
%    L = [0,0,5,=] ;
%    L = [0,+,5,=] ;
%    L = [1,+,4,=] ;
%    L = [1,*,5,=] ;
%    L = [2,+,3,=] ;
%    L = [3,+,2,=] ;
%    L = [4,+,1,=] ;
%    L = [5,+,0,=] ;
%    L = [5,-,0,=] ;
%    L = [5,*,1,=] ;
%    L = [5,/,1,=] ;
%    L = [6,-,1,=] ;
%    L = [7,-,2,=] 
% ----------------------------------------------------------------------

cálculo(N,M,L) :-
   estado_inicial(E1),
   length(L,M),
   acciones_válidas(L),
   transiciones(E1,L,[N,=,=,N]).

% ----------------------------------------------------------------------
% Ejercicio 7. En una subasta se hacen distintas ofertas. Cada oferta
% incluye un lote de productos y un precio por dicho lote. Las ofertas
% realizadas se representan mediante la relación oferta(O,L,P) que se
% verifica si O es una oferta por el lote L con un coste P. Por ejemplo,
% oferta(a,[1,2,3],30) representa la oferta a en la que se puja por el
% lote compuesto por los objetos 1, 2 y 3 por un valor de 30 euros.
%
% Para la aceptación de las ofertas se observan las siguientes reglas:
% + No puede aceptar dos ofertas que contienen un mismo objeto en sus
%   lotes. 
% + Se prefieren las ofertas de mayor ganancia.
% 
% Definir la relación aceptada(-L) que se verifique si L es una lista de
% ofertas aceptadas. Por ejemplo, si las ofertas realizadas se definen
% por
%    oferta(a,[1,2,3],30).
%    oferta(b,[1,2,3],20).
%    oferta(c,[4],20).
%    oferta(d,[2,4],20).
%    oferta(e,[1,2],20).
% entonces,
%    ?- aceptada(L).
%    L = [a, c] 
% ----------------------------------------------------------------------

oferta(a,[1,2,3],30).
oferta(b,[1,2,3],20).
oferta(c,[4],20).
oferta(d,[2,4],20).
oferta(e,[1,2],20).

aceptada(L) :-
   aceptable(L),
   ganancia(L,G),
   not((aceptable(L1), ganancia(L1,G1), G1 > G)).

% aceptable(?L) se verifica si L es una lista de ofertas aceptable; es
% decir, una lista de ofertas que no contienen objetos comunes en sus
% lotes. Por ejemplo, con la definición anterior de ofertas/3,
%    ?- aceptable(L).
%    L = [a, c] ;
%    L = [a] ;
%    L = [b, c] ;
%    L = [b] ;
%    L = [c, e] ;
%    L = [c] ;
%    L = [d] ;
%    L = [e] ;
%    L = [].
aceptable(L) :-
   lista_de_ofertas(L1),
   subconjunto(L,L1),
   es_aceptable(L).

% lista_de_ofertas(-L) se verifica si L es la lista de todas las
% ofertas. Por ejemplo, con la definición anterior de ofertas/3,
%    ?- lista_de_ofertas(L).
%    L = [a, b, c, d, e].
lista_de_ofertas(L) :-
   findall(O,oferta(O,_,_),L).

% es_aceptable(+L) se verifica si la lista de ofertas L es aceptable; es
% decir, no contiene ofertas con objetos comunes en sus lotes. Por
% ejemplo, con la definición anterior de ofertas/3,
%    ?- es_aceptable([c,e]).
%    true.
%    ?- es_aceptable([c,d]).
%    false.
es_aceptable(L) :-
   not(es_inaceptable(L)).

% es_inaceptable(+L) se verifica si L es una lista de ofertas
% inaceptable; es decir, contiene ofertas con objetos comunes en sus
% lotes. Por ejemplo, con la definición anterior de ofertas/3,
%    ?- es_inaceptable([c,d]).
%    true 
%    ?- es_inaceptable([c,e]).
%    false.
es_inaceptable(L) :-
   member(O1,L),
   member(O2,L),
   O1 \= O2,
   oferta(O1,L1,_),
   oferta(O2,L2,_),
   se_solapan(L1,L2).

% se_solapan(+L1,+L2) se verifica si L1 y L2 se solapan; es decir,
% tienen elementos comunes. Por ejemplo,
%    ?- se_solapan([a,b,c],[d,b,e]).
%    true 
%    ?- se_solapan([a,b,c],[d,e]).
%    false.
se_solapan(L1,L2) :-
   member(X,L1),
   member(X,L2).

% ganancia(+L,-G) se verifica si la ganancia de la lista de ofertas L es
% G. Por ejemplo, con la definición anterior de ofertas/3,
%    ?- ganancia([a,c],G).
%    G = 50.
ganancia([],0).
ganancia([O|L],G) :-
   oferta(O,_,G1),
   ganancia(L,G2),
   G is G1+G2.
