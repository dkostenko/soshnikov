#light

// recursive Power function 
let rec pow (x , n) =
  match n with
    | 0.0 -> 1.0
    | n -> x * pow (x, (n-1.0))

// ln(2+x) value table 
let tableOfFunctionValues = [ for x in 0.0..0.1..1.0 -> (x, log(2.0 + x)) ]


let rec superf (x, n, pred) =
  match n with
    | 0.0 -> log(2.0)
    | pred -> superf(x, n-1.0, pred) + pow(-1.0, n - 1.0) * (n-1.0)*x / (2.0*n)

let superfPrint = [for i in 0.0 .. 0.1 .. 1.0 -> (i, superf(i, 9.0, i/2.0)) ]