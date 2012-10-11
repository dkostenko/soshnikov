#light

// возведение в степень
let rec pow (x , n) =
  match n with
    | 0.0 -> 1.0
    | n -> x * pow (x, (n-1.0))
    
    
// Факториал
let rec factorial n = 
  if n = 0.0
  then 1.0
  else n * factorial (n-1.0)


// Разложение в ряд Тейлора
let rec mySh (x, n) =
  match n with
    | 0.0 -> 0.0
    | n -> mySh(x, n - 1.0) + pow(x, 2.0 * n - 1.0) / factorial(2.0 * n - 1.0)
    
// Вывод с помощью получения значения за счет предыдущего
let rec mySinPred (x, n, pred) =
  match n with
    | 0.0 -> 0.0
    | pred -> mySinPred(x, n-1.0, pred)  + pred * x * x / ((n+1.0) * (n+2.0))

// вывод Тейлора
let mySinPrint = [ for i in 0.0 .. 0.1 .. 1.0 -> (i, mySh(i, 10.0)) ]

// вывод значения от предыдущего
let mySinPredPrint = [for i in 0.0 .. 0.1 .. 1.0 -> (i, mySinPred(i, 20.0, 0.0)) ]

// sh(x)
let tableOfFunctionValues = [ for i in 0.0 .. 0.1 .. 1.0 -> (i, sinh(i)) ]