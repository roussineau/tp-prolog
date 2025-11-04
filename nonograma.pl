%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                  %%
%%  Funciones generales implementadas por nosotros  %%
%%                                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%! sublista(?Sub, ?Lista).
sublista([], _).
sublista(S, L) :- append(_, P, L), append(S, _, P), S \= [].

%! headTail(?List, ?Head, ?Tail)
headTail([H|T], H, T).


% Ejercicio 1
%! matriz(F, C, M).
matriz(0,_, []).
matriz(F, C, [M | MS]) :- F > 0, length(M, C), Fm1 is F - 1, matriz(Fm1, C, MS).
% Duda: permite pedir más soluciones, pero está implícito en el esquema recursivo.


% Ejercicio 2
%! replicar(X, N, L).
replicar(_, 0, []).
replicar(X, N, [X | Xs]) :- N > 0, Nm1 is N-1, replicar(X, Nm1, Xs).


% Ejercicio 3
%! transponer(_, _).
transponer([], []).
transponer([[]|_], []).
transponer(M, [Fila|Filas]) :-
    maplist(headTail, M, Fila, Restos),
    transponer(Restos, Filas).

% transponer(Matriz, [Fila | TailFilas]) :-
% 	filaCorrecta(Matriz, Fila),
% 	sacarPrimerColumna(Matriz, MatrizRecortada),
% 	transponer(MatrizRecortada, TailFilas).
% % filaCorrecta(Matriz, Columna).
% filaCorrecta([], []).
% filaCorrecta([[X | _] | TailMatriz], [X | TailFila]) :- 
% 	filaCorrecta(TailMatriz, TailFila).
% % sacarPrimerColumna(Matriz, MatrizRecortada)
% sacarPrimerColumna([], []).
% sacarPrimerColumna([[_ | TailPrimerFila] | TailMatriz], [TailPrimerFila | TailFilas]) :-
% 	sacarPrimerColumna(TailMatriz, TailFilas).


% Predicado dado armarNono/3
armarNono(RF, RC, nono(M, RS)) :-
	length(RF, F),
	length(RC, C),
	matriz(F, C, M),
	transponer(M, Mt),
	zipR(RF, M, RSFilas),
	zipR(RC, Mt, RSColumnas),
	append(RSFilas, RSColumnas, RS).

zipR([], [], []).
zipR([R|RT], [L|LT], [r(R,L)|T]) :- zipR(RT, LT, T).


% Ejercicio 4

% Idea para encarar el ejercicio: vamos a usar el tip 3.
% Dada una pintada válida, podemos armar una lista cuyos primer y último elemento son >= 0,
% y el resto son >=1 intercalando pintadas y espacios en blanco.
% Para armar esta lista, primero vamos a poner los intercalados (los espacios en blanco entre pintadas),
% y luego agregarle las puntas >=0. De eso se encargan los predicados intercalar() y bordear() respectivamente.
% Una vez armada la lista, solo nos queda poner una o (dejar en blanco) las posiciones
% impares (arrancando a contar desde el 1), y poner una x (pintar) las posiciones pares. A efectos
% prácticos, les pusimos simplemente pintarImpar() y pintarPar() a los predicados que se ocupan de esto.

%! pintadasValidas(r(Restricciones, Celdas)).
pintadasValidas(r(Restricciones, Celdas)) :-
	length(Celdas, CantCeldas), % esto seguro que lo tengo que tener, sino no tiene sentido pintar algo que no conozco su longitud
	intercalar(Restricciones, Intercaladas, CantCeldas),
	bordear(Intercaladas, Bordeadas, CantCeldas),
	pintarImpar(Bordeadas, Celdas). % tengo garantizado que Bordeadas va a tener longitud impar.

%! intercalar(Negros, NegrosIntercaladosConBlancos, CantCasilleros)	
intercalar([], [], _).
intercalar([E], [E], _).
intercalar([E1, E2 | Resto], [E1, I1, E2 | RestoIntercalado], CantCasilleros) :-
	P1 is E1 + E2,
	sumlist(Resto, P2),
	Pintados is P1 + P2,
	length([E1, E2 | Resto], CantRestricciones),
	Margen is CantCasilleros + 2 - CantRestricciones,
	Cota is Margen - Pintados,
	between(1, Cota, I1),
	CantCasillerosRestantes is CantCasilleros - E1 - I1,
	intercalar([E2 | Resto], [E2 | RestoIntercalado], CantCasillerosRestantes).

%! bordear(NegrosIntercaladosConBlancos, BlancosIntercaladosConNegros, CantCasilleros).
bordear([], [CantCasilleros], CantCasilleros).
bordear(Restricciones, [B1 | ConCola], CantCasilleros) :-
	sumlist(Restricciones, Ocupadas),
	Margen1 is CantCasilleros - Ocupadas,
	append(Restricciones, [B2], ConCola),
	between(0, Margen1, B1),
	Margen2 is CantCasilleros - Ocupadas - B1,
	between(0, Margen2, B2),
	sumlist([B1 | ConCola], CantCasilleros).

%! pintarPar(Restricciones, Pintado).
pintarPar([], []).
pintarPar([N | Resto], Pintado) :-
	replicar(x, N, Negros),
	pintarImpar(Resto, RestoPintado),
	append(Negros, RestoPintado, Pintado).

% pintarImpar(Restricciones, Pintado).
pintarImpar([B | Resto], Pintado) :-
	replicar(o, B, Blancos),
	pintarPar(Resto, RestoPintado),
	append(Blancos, RestoPintado, Pintado).


% Ejercicio 5
resolverNaive(_) :-  completar("Ejercicio 5").


% Ejercicio 6
pintarObligatorias(_) :- completar("Ejercicio 6").


% Predicado dado combinarCelda/3
combinarCelda(A, B, _) :- var(A), var(B).
combinarCelda(A, B, _) :- nonvar(A), var(B).
combinarCelda(A, B, _) :- var(A), nonvar(B).
combinarCelda(A, B, A) :- nonvar(A), nonvar(B), A = B.
combinarCelda(A, B, _) :- nonvar(A), nonvar(B), A \== B.


% Ejercicio 7
deducir1Pasada(_) :- completar("Ejercicio 7").


% Predicado dado
cantidadVariablesLibres(T, N) :- term_variables(T, LV), length(LV, N).


% Predicado dado
deducirVariasPasadas(NN) :-
	NN = nono(M,_),
	cantidadVariablesLibres(M, VI), % VI = cantidad de celdas sin instanciar en M en este punto
	deducir1Pasada(NN),
	cantidadVariablesLibres(M, VF), % VF = cantidad de celdas sin instanciar en M en este punto
	deducirVariasPasadasCont(NN, VI, VF).


% Predicado dado
deducirVariasPasadasCont(_, A, A). % Si VI = VF entonces no hubo más cambios y frenamos.
deducirVariasPasadasCont(NN, A, B) :- A =\= B, deducirVariasPasadas(NN).


% Ejercicio 8
restriccionConMenosLibres(_, _) :- completar("Ejercicio 8").


% Ejercicio 9
resolverDeduciendo(NN) :- completar("Ejercicio 9").


% Ejercicio 10
solucionUnica(NN) :- completar("Ejercicio 10").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                              %
%    Ejemplos de nonogramas    %
%        NO MODIFICAR          %
%    pero se pueden agregar    %
%                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Fáciles
nn(0, NN) :- armarNono([[1],[2]],[[],[2],[1]], NN).
nn(1, NN) :- armarNono([[4],[2,1],[2,1],[1,1],[1]],[[4],[3],[1],[2],[3]], NN).
nn(2, NN) :- armarNono([[4],[3,1],[1,1],[1],[1,1]],[[4],[2],[2],[1],[3,1]], NN).
nn(3, NN) :- armarNono([[2,1],[4],[3,1],[3],[3,3],[2,1],[2,1],[4],[4,4],[4,2]], [[1,2,1],[1,1,2,2],[2,3],[1,3,3],[1,1,1,1],[2,1,1],[1,1,2],[2,1,1,2],[1,1,1],[1]], NN).
nn(4, NN) :- armarNono([[1, 1], [5], [5], [3], [1]], [[2], [4], [4], [4], [2]], NN).
nn(5, NN) :- armarNono([[], [1, 1], [], [1, 1], [3]], [[1], [1, 1], [1], [1, 1], [1]], NN).
nn(6, NN) :- armarNono([[5], [1], [1], [1], [5]], [[1, 1], [2, 2], [1, 1, 1], [1, 1], [1, 1]], NN).
nn(7, NN) :- armarNono([[1, 1], [4], [1, 3, 1], [5, 1], [3, 2], [4, 2], [5, 1], [6, 1], [2, 3, 2], [2, 6]], [[2, 1], [1, 2, 3], [9], [7, 1], [4, 5], [5], [4], [2, 1], [1, 2, 2], [4]], NN).
nn(8, NN) :- armarNono([[5], [1, 1], [1, 1, 1], [5], [7], [8, 1], [1, 8], [1, 7], [2, 5], [7]], [[4], [2, 2, 2], [1, 4, 1], [1, 5, 1], [1, 8], [1, 7], [1, 7], [2, 6], [3], [3]], NN).
nn(9, NN) :- armarNono([[4], [1, 3], [2, 2], [1, 1, 1], [3]], [[3], [1, 1, 1], [2, 2], [3, 1], [4]], NN).
nn(10, NN) :- armarNono([[1], [1], [1], [1, 1], [1, 1]], [[1, 1], [1, 1], [1], [1], [ 1]], NN).
nn(11, NN) :- armarNono([[1, 1, 1, 1], [3, 3], [1, 1], [1, 1, 1, 1], [8], [6], [10], [6], [2, 4, 2], [1, 1]], [[2, 1, 2], [4, 1, 1], [2, 4], [6], [5], [5], [6], [2, 4], [4, 1, 1], [2, 1, 2]], NN).
nn(12, NN) :- armarNono([[9], [1, 1, 1, 1], [10], [2, 1, 1], [1, 1, 1, 1], [1, 10], [1, 1, 1], [1, 1, 1], [1, 1, 1, 1, 1], [1, 9], [1, 2, 1, 1, 2], [2, 1, 1, 1, 1], [2, 1, 3, 1], [3, 1], [10]], [[], [9], [2, 2], [3, 1, 2], [1, 2, 1, 2], [3, 11], [1, 1, 1, 2, 1], [1, 1, 1, 1, 1, 1], [3, 1, 3, 1, 1], [1, 1, 1, 1, 1, 1], [1, 1, 1, 3, 1, 1], [3, 1, 1, 1, 1], [1, 1, 2, 1], [11], []], NN).
nn(13, NN) :- armarNono([[2], [1,1], [1,1], [1,1], [1], [], [2], [1,1], [1,1], [1,1], [1]], [[1], [1,3], [3,1,1], [1,1,3], [3]], NN).
nn(14, NN) :- armarNono([[1,1], [1,1], [1,1], [2]], [[2], [1,1], [1,1], [1,1]], NN).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                              %
%    Predicados auxiliares     %
%        NO MODIFICAR          %
%                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%! completar(+S)
%
% Indica que se debe completar el predicado. Siempre falla.
completar(S) :- write("COMPLETAR: "), write(S), nl, fail.

%! mostrarNono(+NN)
%
% Muestra una estructura nono(...) en pantalla
% Las celdas x (pintadas) se muestran como ██.
% Las o (no pintasdas) se muestran como ░░.
% Las no instanciadas se muestran como ¿?.
mostrarNono(nono(M,_)) :- mostrarMatriz(M).

%! mostrarMatriz(+M)
%
% Muestra una matriz. Solo funciona si las celdas
% son valores x, o, o no instanciados.
mostrarMatriz(M) :-
	M = [F|_], length(F, Cols),
	mostrarBorde('╔',Cols,'╗'),
	maplist(mostrarFila, M),
	mostrarBorde('╚',Cols,'╝').

mostrarBorde(I,N,F) :-
	write(I),
	stringRepeat('══', N, S),
	write(S),
	write(F),
	nl.

stringRepeat(_, 0, '').
stringRepeat(Str, N, R) :- N > 0, Nm1 is N - 1, stringRepeat(Str, Nm1, Rm1), string_concat(Str, Rm1, R).

%! mostrarFila(+M)
%
% Muestra una lista (fila o columna). Solo funciona si las celdas
% son valores x, o, o no instanciados.
mostrarFila(Fila) :-
	write('║'),
	maplist(mostrarCelda, Fila),
	write('║'),
	nl.

mostrarCelda(C) :- nonvar(C), C = x, write('██').
mostrarCelda(C) :- nonvar(C), C = o, write('░░').
mostrarCelda(C) :- var(C), write('¿?').
