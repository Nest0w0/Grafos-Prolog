/*Los grafos de este programa se construyen con hechos para las aristas
aristas(x,y,p) :- donde x es el nodo de inicio de la arista
                  y es el nodo final de la arista
                  p es el peso de la arista

Por defecto, el programa trata al grafo como no dirigido. Para habilitarlo para
grafos dirigidos, basta con comentar la l�nea 21*/
arista(a,c,1).
arista(a,d,1).
arista(a,e,1).

arista(b,a,1).
arista(b,c,1).
arista(b,e,1).

arista(c,d,1).
arista(e,d,1).


conexion(X,Y,Z) :- arista(X,Y,Z).
conexion(X,Y,Z) :- arista(Y,X,Z).
%???????? Comentar esta linea para grafos dirigidos

/*Funci�n que determina a un sendero (walk) entre un nodo y otro.
Enti�ndase como sendero a una secuencia de nodos y aristas, en las que ambos se pueden repetir.
En un grafo no dirigido hay infinitos senderos.

Estos pueden ser abiertos (el nodo de inicio y fin son diferentes).

sendero_abierto(A,B,L) :- A es el nodo de inicio, B es el nodo final y L es la lista que contiene el sendero*/

sendero_abierto(B,B,[B]) :- !.
sendero_abierto(A,B,[A,B]) :- conexion(A,B,_).
sendero_abierto(A,B,[A|L]) :- conexion(A,X,_), sendero_abierto(X,B,L).

/*O cerrados (el nodo de inicio y final son iguales)

sendero_cerrado(A,L) :- A es el nodo de inicio y final, L es la lista que contiene el sendero*/
sendero_cerrado(A,[A|L]) :- conexion(A,X,_) , sendero_abierto(X,A,L).



/*Funci�n que determinar un camino (path) entre un nodo y otro.
Entiendase como camino a una secuencia de nodos y aristas, en las que ninguno de los dos se puedne repetir.
En la literatura anglosajona, se les llama "Path".

camino(A,B,L):- A es el nodo de inicio del camino, B es el nodo final del camino, L es la lista que contiene el camino.*/
camino(A,B,L) :- camino(A,B,L,[]).
camino(B,B,[B],_) :- !.
camino(A,B,[A|L],Z) :- not(member(A,Z)),conexion(A,X,_), camino(X,B,L,[A|Z]).



/*Funci�n que determina un ciclo (cycle).
Enti�ndase como ciclo a una secuencia de nodos y aristas, en las que ni nodos ni aristas se pueden repetir.
La �nica excepci�n es el nodo de inicio y final, que deben el mismo en la secuencia.
Un ciclo tambi�n puede interpretarse como un camino cerrado.

ciclo(A,L) :- A es el nodo de inicio y final, L es la lista que contiene el ciclo*/
ciclo(A,[A|L]) :- conexion(A,X,_), camino(X,A,L), L \= [X,A].
%Todos los ciclos son circuitos, pero no todos los circuitos son ciclos.



/*Funci�n que determina un rastro (trail) entre un nodo y otro.
Enti�ndase como rastro a una secuencia de nodos y aristas, en las que
los nodos se pueden repetir pero las aristas no.

En literatura anglosajona, se les llama "Trail"

rastro(A,B,L) :- A es el nodo de inicio, B es el nodo final y L es la lista que contiene le rastro*/
rastro(A,B,L) :- rastro(A,B,L,[]).
rastro(A,A,[A|L],V) :- conexion(A,X,P), not(member((A,X,P),V) ; member((X,A,P),V)), rastro(X,A,L,[(A,X,P)|V]).
rastro(B,B,[B],_) :- !.
rastro(A,B,[A|L],V) :- conexion(A,X,P), not(member((A,X,P),V) ; member((X,A,P),V)), rastro(X,B,L,[(A,X,P)|V]).



/*Funci�n que determina un circuito (circuito).
Enti�ndase como circuito a una secuencia de nodos y aristas, que empieza y
termina en el mismo nodo, y sus aristas no se pueden repetir.
Un circuito puede interpretarse como un rastro cerrado.

En la literatura anglosajona, se les llama "Circuit".
circuito(A,L) :- A es el nodo de inicio y final, L es la lista que contiene el circuito*/
circuito(A,[A|L]) :- conexion(A,X,P), rastro(X,A,L,[(A,X,P)]).