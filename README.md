# Interactive Set Language

This is a proposal for final project for the programming language analysis course.

## How to run

    stack setup
    stack build
    stack exec TPFinal-exe

## About

Para mi projecto final, se propone la creación de un lenguaje de dominio especifico
que permite el procesamiento de conjuntos, llamado lenguaje de conjuntos
interactivo (ICL), esto se hace de la siguiente manera:
* se definen las operaciones a+b, a-b, a^b que son la unión, resta e intersección de conjuntos respectivamente
* se definen los operadores "-" unario, "print" e "in", el primero representa el complemento,
  el segundo imprime un conjunto, y el in permite saber si un elemento pertenece a un conjunto
* se definen los constructores [] y {}, el primero crea un conjunto cerrado [a; b]
  y el segundo crea un conjunto por sus elementos {a; b; c; d}
* se define el operador "exit" que termina la ejecución
* se definen los valores v como un flotante que puede ser negativos

Este lenguaje es interpretado y se ejecuta desde terminal, un ejemplo:

    ISL> x = [1.0, 2.0]
    ISL> y = -x
    ISL> print y
    [-inf *=* 1.0, 2.0 *=* inf]

Para implementar este TP final se requiere el uso de:
* Parsec (https://hackage.haskell.org/package/parsec) para parsear la entrada
* Data.Range (https://hackage.haskell.org/package/range-0.3.0.2/docs/Data-Range.html)
* y otros: Data.Map.Strict, Data.Maybe, Prelude
* Tambien se usa stack para obtener dependencias

Manejo de errores:
* Para errores de sintaxis se emite el error de Parsec, que es muy detallado respecto
  de lo que se esperaba.
* (no implementado) Para conjuntos se avisa cuando un conjunto es vacio pero no
  se ingreso con el constructor "{}", por ejemplo "x = [3.0, 2.0]" es vacio, pero probablemente
  un error de tipeo, entonces se emite: "Warning: x is empty but {} was not used to declare it".
* Para manejo de errores de variables no declaradas se emite "Error: x not declared".

## Examples

```
ISL> a = {}
ISL> print a
[]
ISL> b = {1.0}
ISL> print b
[SingletonRange 1.0]
ISL> c = {1.0, 2.0}
ISL> print c
[SingletonRange 1.0, SingletonRange 2.0]
ISL> d = [3.0, 4.0]
ISL> print d
[3.0 +=+ 4.0]
ISL> e = c + d
ISL> print e
[SingletonRange 1.0, SingletonRange 2.0, 3.0 +=+ 4.0]

```
