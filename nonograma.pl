% Ejercicio 1
%! matriz(+F, +C, -M).
matriz(0,_, []).
matriz(F, C, [M | MS]) :- F > 0, length(M, C), Fm1 is F - 1, matriz(Fm1, C, MS).


% Ejercicio 2
%! replicar(+Elem, +N, -Lista).
replicar(_, 0, []).
replicar(X, N, [X | Xs]) :- N > 0, Nm1 is N-1, replicar(X, Nm1, Xs).
% Ejercicio 12: en texttt.md


% Ejercicio 3
%! transponer(+M, -MT).
transponer([], []).
transponer([[]|_], []).
transponer(M, [Fila | Filas]) :-
    maplist(headTail, M, Fila, Restos),
    transponer(Restos, Filas).

%! headTail(?List, ?Head, ?Tail)
headTail([H | T], H, T).


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
/*
	Idea para encarar el ejercicio: vamos a usar el tip 3.
	Dada una pintada válida, podemos armar una lista cuyos primer y último elemento son >= 0,
	y el resto son >=1 intercalando pintadas y espacios en blanco.
	Para armar esta lista, primero vamos a poner los intercalados (los espacios en blanco entre pintadas),
	y luego agregarle las puntas >=0. De eso se encargan los predicados intercalarBlancos() y bordesBlancos() respectivamente.
	Una vez armada la lista, solo nos queda poner una o (dejar en blanco) las posiciones
	impares (arrancando a contar desde el 1), y poner una x (pintar) las posiciones pares. A efectos
	prácticos, les pusimos simplemente pintarBlanco() y pintarNegro() a los predicados que se ocupan de esto.
*/

%! pintadasValidas(+R).
pintadasValidas(r(Negros, Celdas)) :-
	length(Celdas, CantCeldas), % esto seguro que lo tengo que tener, sino no tiene sentido pintar algo que no conozco su longitud
	intercalarBlancos(Negros, CantCeldas, Intercaladas),
	bordesBlancos(Intercaladas, CantCeldas, Bordeadas),
	pintarBlanco(Bordeadas, Celdas). % tengo garantizado que Bordeadas va a tener longitud impar.

%! intercalarBlancos(Negros, NegrosIntercaladosConBlancos, CantCeldas)	
intercalarBlancos([], _, []).
intercalarBlancos([N], _, [N]).
intercalarBlancos([N1, N2 | Resto], CantCeldas, [N1, B1, N2 | RestoIntercalado]) :-
	sumlist([N1, N2 | Resto], Pintados),
	length([N1, N2 | Resto], CantRestricciones),
	Cota is CantCeldas + 2 - CantRestricciones - Pintados,
	between(1, Cota, B1),
	CantCeldasRestantes is CantCeldas - N1 - B1,
	intercalarBlancos([N2 | Resto], CantCeldasRestantes, [N2 | RestoIntercalado]).

%! bordesBlancos(NegrosIntercaladosConBlancos, BlancosIntercaladosConNegros, CantCeldas).
bordesBlancos([], CantCeldas, [CantCeldas]).
bordesBlancos(Restricciones, CantCeldas, [B1 | ConCola]) :-
	sumlist(Restricciones, Ocupadas),
	Margen1 is CantCeldas - Ocupadas,
	between(0, Margen1, B1),
	Margen2 is CantCeldas - Ocupadas - B1,
	between(0, Margen2, B2),
	append(Restricciones, [B2], ConCola),
	sumlist([B1 | ConCola], CantCeldas).

%! pintarNegro(Restricciones, Pintado).
pintarNegro([], []).
pintarNegro([N | Resto], Pintado) :-
	replicar(x, N, Negros),
	pintarBlanco(Resto, RestoPintado),
	append(Negros, RestoPintado, Pintado).

%! pintarBlanco(Restricciones, Pintado).
pintarBlanco([B | Resto], Pintado) :-
	replicar(o, B, Blancos),
	pintarNegro(Resto, RestoPintado),
	append(Blancos, RestoPintado, Pintado).


% Ejercicio 5
%! resolverNaive(+NN).
resolverNaive(nono(_, Restricciones)) :-
	maplist(pintadasValidas, Restricciones).


% Ejercicio 6
%! pintarObligatorias(+R).
pintarObligatorias(r(Negros, PintadasObligatorias)) :- 
	listaPintadasValidas(r(Negros, PintadasObligatorias), Pintadas),
	transponer(Pintadas, PT),
	maplist(combinarCeldas, PT, PintadasObligatorias).

%! listaPintadasValidas(?R, ?L).
listaPintadasValidas(Restriccion, ListaPintadasValidas) :-
	bagof(Restriccion, pintadasValidas(Restriccion), ListaRestricciones),
	maplist(pintada, ListaRestricciones, ListaPintadasValidas).

%! combinarCeldas(+Celdas, -Celda).
combinarCeldas([C], C).
combinarCeldas([C1, C2 | Cs], C) :-
	combinarCelda(C1, C2, C3),
	combinarCeldas([C3 | Cs], C).

% pintada(?R, ?L).
pintada(r(_, L), L).


% Predicado dado combinarCelda/3
combinarCelda(A, B, _) :- var(A), var(B).
combinarCelda(A, B, _) :- nonvar(A), var(B).
combinarCelda(A, B, _) :- var(A), nonvar(B).
combinarCelda(A, B, A) :- nonvar(A), nonvar(B), A = B.
combinarCelda(A, B, _) :- nonvar(A), nonvar(B), A \== B.


% Ejercicio 7
%! deducir1Pasada(+NN).
deducir1Pasada(nono(_, RS)) :-
	maplist(pintarObligatorias, RS).


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
%! restriccionConMenosLibres(+NN, -R).
restriccionConMenosLibres(nono(_, Rs), R) :-
	member(R, Rs), cantidadVariablesLibres(R, CR), CR > 0,
	not((member(R2, Rs), cantidadVariablesLibres(R2, CR2), CR2 > 0, CR2 < CR)).


% Ejercicio 9
%! resolverDeduciendo(+NN).
resolverDeduciendo(NN) :-
	deducirVariasPasadas(NN),
	cantidadVariablesLibres(NN, C),
	resolverDeduciendoCont(NN, C),
	deducirVariasPasadas(NN).

%! resolverDeduciendoCont(+NN, +CantFreeVars).
resolverDeduciendoCont(_, 0).
resolverDeduciendoCont(NN, FV) :-
	FV > 0,
	restriccionConMenosLibres(NN, R), !,
	pintadasValidas(R),
	cantidadVariablesLibres(NN, FVN),
	resolverDeduciendoCont(NN, FVN).


% Ejercicio 10
%! solucionUnica(+NN).
solucionUnica(NN) :- bagof(NN, resolverDeduciendo(NN), L), length(L, 1).


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


%! tam(+N, -T).
tam(N, (F, C)) :- nn(N, nono([M | Ms], _)), length([M | Ms], F), length(M, C).
