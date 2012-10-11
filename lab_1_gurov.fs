#light

// recursive Power function 
let rec pow (x , n ) =
  match n with
    | 0.0 -> 1.0
    | n -> x * pow (x, (n-1.0))

// Tailor series computation
let rec taylorSeries (x, n) =
  match n with
    | 0.0 -> 0.693147181
    | n -> taylorSeries(x, n-1.0) + pow(-1.0, n+1.0) * pow(x, n) / (n * pow(2.0, n)) 

// ln(2+x) value table 
let tableOfFunctionValues = [ for i in -10.0 .. 10.0 -> (i/10.0, log(2.0+(i/10.0))) ]

// and now using the Tailor series
let taylorValuesTable = [ for i in -10.0 .. 10.0 -> (i/10.0, taylorSeries((i/10.0), 30.0)) ]
