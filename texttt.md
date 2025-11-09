# Ejercicio 11: Análisis de nonogramas

|N |Tamaño|¿Tiene solución única?   |¿Es deducible sin backtracking?|
|--|------|-------------------------|-------------------------------|
|0 |2x3   |Sí (deducirVariasPasadas)|Sí (deducirVariasPasadas)      |
|1 |5x5   |Sí (deducirVariasPasadas)|Sí (deducirVariasPasadas)      |
|2 |5x5   |Sí (deducirVariasPasadas)|Sí (deducirVariasPasadas)      |
|3 |10x10 |Sí (deducirVariasPasadas)|Sí (deducirVariasPasadas)      |
|4 |5x5   |Sí (deducirVariasPasadas)|Sí (deducirVariasPasadas)      |
|5 |5x5   |Sí (resolverDeduciendo)  |No (deducirVariasPasadas)      |
|6 |5x5   |Sí (deducirVariasPasadas)|Sí (deducirVariasPasadas)      |
|7 |10x10 |Sí (deducirVariasPasadas)|Sí (deducirVariasPasadas)      |
|8 |10x10 |Sí (deducirVariasPasadas)|Sí (deducirVariasPasadas)      |
|9 |5x5   |Sí (deducirVariasPasadas)|Sí (deducirVariasPasadas)      |
|10|5x5   |No (resolverDeduciendo)  |No (deducirVariasPasadas)      |
|11|10x10 |Sí (deducirVariasPasadas)|Sí (deducirVariasPasadas)      |
|12|15x15 |Sí (deducirVariasPasadas)|Sí (deducirVariasPasadas)      |
|13|11x5  |Sí (resolverDeduciendo)  |No (deducirVariasPasadas)      |
|14|4x4   |Sí (resolverDeduciendo)  |No (deducirVariasPasadas)      |

Dado un nonograma predefinido iésimo, cuando `?- nn(i, NN), deducirVariasPasadas(NN), mostrarNono(NN).` nos mostraba un nonograma sin variables libres sabíamos que este nonograma era deducible sin backtracing, y en consecuencia era de solución única. En caso contrario (cuando nos mostraba uno con variables libres), probábamos la consulta `?- nn(i, NN), resolverDeduciendo(NN), mostrarNono(NN).` y ahí verificábamos si tenía una única solución.

El tamaño lo calculamos a mano, y luego leímos la consigna 😄 y verificamos con la consulta `?- tam(i, T).`, no sin antes corregir la versión del predicado dada por la consigna, ya que nuestro predicado `matriz/3` no es reversible en el primer parámetro. 

# Ejercicio 12: Reversibilidad

### Indicar si el predicado `replicar/3` es reversible en el segundo argumento. En concreto se pide analizar si `replicar(+Elem, -N, -Lista)` funciona correctamente.

```prolog
%! replicar(+X, +N, -L).
replicar(_, 0, []).
replicar(X, N, [X | Xs]) :- N > 0, Nm1 is N-1, replicar(X, Nm1, Xs).
```

Nuestra implementación de replicar no es reversible en el parámetro N, ya que la primera cláusula de la segunda regla utiliza el motor aritmético de Prolog para hacer la comparación `N > 0`, y este requiere que ambos parámetros sean expresiones aritméticas. En particular, como no estaría instanciada, N no sería ni un número, ni una variable ya instanciada en una expresión aritmética, ni suma, resta, multiplicación o división de expresiones aritméticas.