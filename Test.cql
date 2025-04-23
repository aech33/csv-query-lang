def fact n := if n == 0 then 1 else n * fact (n - 1)
def add x y := x + y

def append xs ys :=
  match xs with
  | [] => ys
  | x :: xs' => x :: append xs' ys
  end

def filter f ls :=
  match ls with
  | [] => []
  | x :: xs => if f x then x :: filter f xs else filter f xs
  end

def odd  n := if n == 0 then false else even (n - 1)
def even n := if n == 0 then true  else odd  (n - 1)

def isInt n := match toInt n with
  | #Some x => true
  | x => false
  end

def main :=
  let csv := load "CSVTest1.csv" in
  printCSV (for x : csv @ i j => isInt x)
