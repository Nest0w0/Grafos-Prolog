/*Los grafos de este programa se construyen con hechos para las aristas

aristas(x,y,p) :- donde x es el nodo de inicio de la arista
                  y es el nodo final de la arista
                  p es el peso de la arista

Por defecto, el programa trata al grafo como no dirigido. Para habilitarlo para
grafos dirigidos, basta con comentar el segundo predicado de la regla "conexion"*/


arista(a,b,4).
arista(a,d,6).
arista(a,g,7).
arista(a,f,4).
arista(b,d,9).
arista(b,c,2).
arista(c,d,6).
arista(c,e,1).
arista(c,f,4).
arista(g,f,10).
arista(f,e,3).


/*
arista(a,c,2).
arista(a,e,3).
arista(b,e,9).
arista(d,b,8).
arista(c,d,3).
*/

conexion(X,Y,Z) :- arista(X,Y,Z).
conexion(X,Y,Z) :- arista(Y,X,Z). % <---- Comentar esta linea para grafos dirigidos

/*Función que determina a un sendero (walk) entre un nodo y otro.
Entiéndase como sendero a una secuencia de nodos y aristas, en las que ambos se pueden repetir.
En un grafo no dirigido hay infinitos senderos.

Estos pueden ser senderos abiertos (el nodo de inicio y fin son diferentes).

sendero_abierto(A,B,L,P) :- A es el nodo de inicio,
                            B es el nodo final,
                            L es la lista que contiene el sendero,
                            P es el peso del sendero.*/
sendero_abierto(B,B,[B],0) :- !.
sendero_abierto(A,B,[A,B],Z) :- conexion(A,B,Z).
sendero_abierto(A,B,[A|L],P) :- conexion(A,X,Z), sendero_abierto(X,B,L,Ps), P is Ps + Z.


/*O senderos cerrados (el nodo de inicio y final son iguales)

sendero_cerrado(A,L,P) :- A es el nodo de inicio y final,
                          L es la lista que contiene el sendero,
                          P es el peso del sendero.*/
sendero_cerrado(A,[A|L],P) :- conexion(A,X,Z) , sendero_abierto(X,A,L,Ps), P is Ps + Z.



/*Función que determinar un camino (path) entre un nodo y otro.
Entiendase como camino a una secuencia de nodos y aristas, en las que ninguno de los dos se puedne repetir.
En la literatura anglosajona, se les llama "Path".

camino(A,B,L,P):- A es el nodo de inicio del camino,
                  B es el nodo final del camino,
                  L es la lista que contiene el camino,
                  P es el peso del camino.*/
camino(A,B,L,P) :- camino(A,B,L,[],P).
camino(B,B,[B],_,0) :- !.
camino(A,B,[A|L],V,P) :- not(member(A,V)),conexion(A,X,Z), camino(X,B,L,[A|V],Ps), P is Ps + Z.



/*Función que determina un ciclo (cycle).
Entiéndase como ciclo a una secuencia de nodos y aristas, en las que ni nodos ni aristas se pueden repetir.
La única excepción es el nodo de inicio y final, que deben el mismo en la secuencia.
Un ciclo también puede interpretarse como un camino cerrado.

ciclo(A,L,P) :- A es el nodo de inicio y final,
              L es la lista que contiene el ciclo,
              P es el peso del ciclo.*/
ciclo(A,[A|L],P) :- conexion(A,X,Z), camino(X,A,L,Ps), L \= [X,A], P is Ps + Z.      %Todos los ciclos son circuitos, pero no todos los circuitos son ciclos.



/*Función que determina un rastro (trail) entre un nodo y otro.
Entiéndase como rastro a una secuencia de nodos y aristas, en las que
los nodos se pueden repetir pero las aristas no.

rastro(A,B,L,P) :- A es el nodo de inicio,
                 B es el nodo final,
                 L es la lista que contiene le rastro,
                 P es el peso del rastro.*/
rastro(A,B,L,P) :- rastro(A,B,L,[],P).
rastro(A,A,[A|L],V,P) :- conexion(A,X,Z), not(member((A,X,Z),V) ; member((X,A,Z),V)), rastro(X,A,L,[(A,X,Z)|V],Ps), P is Ps + Z.
rastro(B,B,[B],_,0) :- !.
rastro(A,B,[A|L],V,P) :- conexion(A,X,Z), not(member((A,X,Z),V) ; member((X,A,Z),V)), rastro(X,B,L,[(A,X,Z)|V],Ps), P is Ps + Z.



/*Función que determina un circuito (circuit).
Entiéndase como circuito a una secuencia de nodos y aristas, que empieza y
termina en el mismo nodo, y sus aristas no se pueden repetir.
Un circuito puede interpretarse como un rastro cerrado.

circuito(A,L,) :- A es el nodo de inicio y final,
                 L es la lista que contiene el circuito,
                 P es el peso del circuito.*/
circuito(A,[A|L],P) :- conexion(A,X,Z), rastro(X,A,L,[(A,X,Z)],Ps), P is Ps + Z.


cHamiltoniano(A,[A|L]) :- nodos(N), long(N,M), ciclo(A,[A|L],_), long(L,K), K = M.


%--------FUNCIONES AUXILIARES-------
nodos(L) :- nodos([],L).
nodos(X,L) :- (arista(A,_,_) ; arista(_,A,_)),
            not(member(A,X)),
            nodos([A|X],L),!.
nodos(X,L) :- L = X,!.

/*4-Calcula la longitud de la lista*/
long([],0):-!.
long([_|Y],S):-long(Y,T),!, S is T + 1.

