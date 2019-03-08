# Nota sobre el pseudocódigo {.unnumbered}

Para la descripción de los algoritmos utilizo pseudocódigo basado en una versión simplificada de Haskell.
Creo que es más adecuado que una versión imperativa ya que refleja mejor el código original.

Omito en el pseudocódigo la gestión explícita de los generadores de números aleatorios.

Los argumentos se pasan a las funciones separados por espacios y el tipo de la función se indica después de su nombre seguido de dos dobles puntos `::`. 

Para mostrar el estilo del pseudocódigo incluyo un ejemplo de pseudocódigo para una función en C:

```C
int suma(int a, int b){
  int c = a + b;
  return c;
}
```


Y su equivalente en el estilo de pseudocódigo que voy a utilizar:

```haskell
suma :: Int → Int → Int
suma a b = c
  where c = a + b
```

Describo a continuación algunas funciones comunes utilizadas en más de un pseudocódigo:

- `juntaCon f l1 l2 ... ln` toma n listas y una función de n argumentos y devuelve una lista tal que la posición `i` tiene el elemento `f l1_i l2_i ... ln_i`
- `map f [x1, ..., xn]` toma una función `f` y una lista `[x1, ..., xn]` y devuelve la lista `[f x1, ..., f xn]`
- `acumula (·) i [x1, ..., xn]` acumula los elementos de la lista usando la función `·`. Devuelve: `x1·x2···xn·i`. Es equivalente a la función `accumulate` de C++.
- `\x1 x2 ... xn → expr` es una función anónima que toma `x1 x2 ... xn` como argumentos y devuelve `expr`
- `repite n x` crea una lista de n copias de `x`. Si `x` devuelve resultados aleatorios cada resultado será generado aleatoriamente.

# Descripción del problema

Dado $n \in \mathbb{N}$ y un conjunto de clases $C$ un *clasificador* es una aplicación $f:\mathbb{R}^n \to C$ que asigna a cada vector de datos $v \in \mathbb{R}^n$ una clase $f(v)$.

El problema de clasificación consiste en, dado un conjunto de $m$ datos ya clasificados $D = \{(w,\nu(w)) \;:\; w \in \mathbb{R}^n, \nu(w) \in C\}$ obtener un clasificador que permita clasificar otros ejemplos de forma automática. Se dice que el clasificador *ha sido entrenado* con los datos $D$.

Dado $k \leq m$ impar el clasificador $k$-NN asigna a cada $v \in \mathbb{R}^n$ la clase que más se repita entre los $k$ vecinos más cercanos de $v$ (es decir, aquellos a menor distancia para una distancia dada). 

Para nuestro problema consideramos $1$-NN, es decir, tomamos la clase del vecino más cercano. La distancia considerada es la euclídea si las características son continuas y la distancia trivial (esto es, $d_h(x,y) = 1$ si $x \neq y$ o 0 si son iguales) para características discretas **ponderada por un vector de pesos $w$**. Es decir, si tenemos $u,v \in \mathbb{R}^n \times C_1 \cdots C_m$ con $C_i$ un conjunto de clases y $w \in [0,1]^{n+m}$ pesos tenemos que su distancia se define:

$$d_w(u,v) = \sqrt{\sum_i w_i(u_i - v_i)^2 + \sum_j w_jd_h(u_j,v_j)}$$

Fijado un problema de clasificación podemos medir dos características de los vectores de pesos:

 - Su **precisión** ($T_{\operatorname{prec}}$): si utilizamos un clasificador $1$-NN con distancia $d_w$, ¿cómo de *bueno* es el clasificador? 
   Para ello medimos cuántos vectores se clasifican correctamente si entrenamos el clasificador con el resto de datos (*leave one out*).
- Su **simplicidad** ($T_{\operatorname{simpl}}$): Las características con un peso asociado menor tendrán menos importancia en la clasificación. ¿Cuántas características pesan *poco*? Para ello contamos aquellas características que valgan menos de 0.2

El aprendizaje de pesos en características consiste entonces en hallar el vector de pesos que maximize la precisión y simplicidad, es decir, hallar $w \in [0,1]^{n+m}$ que maximice:

$$F(w) = \alpha T_{\operatorname{prec}}(w) + (1 - \alpha) T_{\operatorname{simpl}}(w)$$

# Descripción de la aplicación de los algoritmos

En este apartado se describen las consideraciones, tipos y operaciones comunes a los algoritmos de cada práctica.

Léase la sección [Nota sobre el pseudocódigo] para ver consideraciones comunes a todos los trozos de pseucódigo.

## Esquemas de representación

Los **datos** de entrada se representan internamente con los siguientes datos:

- Una **característica** (`Feature`) es un `Double` (corresponde al tipo `double` de C). Es una de las características de entrada.
- Un **vector de características** (`Point`) es un vector tipo C de `Feature`s.
- Una **clase** (`Class`) es un `Int` (corresponde al tipo `long int` de C). 
- Un **ejemplo** (`Example`) es un par que tiene un vector de características y una clase. Representa un ejemplo completo, del que conocemos su clase. Las funciones `claseDe` y `puntoDe` obtienen la clase y vector de características asociados a un ejemplo.
- Un **dataset** (`DataSet`) es una lista de ejemplos. El programa transforma un fichero `.arff` a un `DataSet`.
- Un **algoritmo** (`Algorithm`) es una función que dado un `DataSet` devuelve una solución.

Las **soluciones** (vectores de pesos llamados `Weights`) se representan como un vector tipo C de `Double`s.
Para los algoritmos de esta práctica estos vectores se incluyen en otra estructura `Solution` que contiene:

- El número de solución (de entre las generadas)
- Su valor para la función objetivo
- El vector de pesos

Los elementos de tipo `Solution` pueden compararse y un elemento es mayor que otro si su valor para la función objetivo es mayor.

En los algoritmos genéticos y meméticos las soluciones están en una **población**: un conjunto que mantiene el orden de las soluciones. Recibe el nombre de `Population`.


## Operadores comunes (1-NN)

En esta sección describimos los operadores que se utilizan en todos los algoritmos o en su evaluación.

La función distancia ponderada calcula la distancia al cuadrado. Como la raíz cuadrada es una función creciente para calcular mínimos o máximos el resultado es equivalente.

```haskell
dist :: Weights → Point → Point → Double
dist ws xs ys = suma (juntaCon (\w x y → w*(x-y)²) ws xs ys)
```

La función clasifica toma un vector de pesos, un dataset y un punto y clasifica el punto en función del dataset y el vector de puntos utilizando el algoritmo de vecino más cercano. `mínimoCon` nos da el mínimo utilizando una función de comparación dada.

```haskell
clasifica :: Weights → DataSet → Point → Class
clasifica ws dataSet punto = claseDe másCercano
  where másCercano = mínimoCon (compara (dist ws punto . puntoDe)) dataSet
```

La función norma descarta los pesos menores que 0.2:

```haskell
norma :: Weights → Weights
norma ws = map (\x → if x < 0.2 then 0 else x) ws
```

## Función objetivo

La función objetivo es la suma ponderada de precisión y simplicidad. Como la ponderación es la misma ($\alpha$ es 0.5), simplemente hacemos la suma para simplificar los cálculos:

```haskell

objetivo :: Weights → DataSet → Double
objetivo ws ds = precision (norma ws) ds + simplicidad ws
```

La precisión cuenta el número de aciertos. `ds\\a` indica el dataset `ds` eliminando el elemento `a` (para hacer *leave one out*).

```haskell
precision :: Weights → DataSet → Double
precision ws ds = acumula cuenta 0 ds/longitud ds
  where cuenta a acc = acc + (clasifica ws (ds\\a) (puntoDe a) == claseDe a)
```

La simplicidad es la proporción de pesos del total que tienen:

```haskell
simplicidad :: Weights → Double
simplicidad ws = m/n
  where m = acumula (\w acc → acc + (w < 0.2)) 0 ws
        n = longitud ws
```
