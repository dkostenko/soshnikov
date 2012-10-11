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
let rec mySin (x, n) =
  match n with
    | 0.0 -> 0.0
    | n -> mySin(x, n - 1.0) + pow(-1.0, n - 1.0) * pow(2.0, 2.0 * n - 1.0) * pow(x, (n * 2.0)) / factorial(2.0 * n)
    
// Вывод с помощью получения значения за счет предыдущего
let rec mySinPred (x, n) =
  match n with
    | 0.0 -> 0.0
    | n -> mySinPred(x, n - 1.0) + pow(-1.0, n - 1.0) * 4.0 * x * x / (10.0 - n - 1.00001) * (10.0 - n - 2.00001)

// вывод Тейлора
let mySinPrint = [ for i in 0.0 .. 0.1 .. 1.0 -> (i, mySin(i, 10.0)) ]

// вывод значения от предыдущего
let mySinPredPrint = [for i in 0.0 .. 0.1 .. 1.0 -> (i, mySinPred(i, 10.0)) ]


// sin(x)^2
let tableOfFunctionValues = [ for i in 0.0 .. 0.1 .. 1.0 -> (i, sin(i)*sin(i)) ]


