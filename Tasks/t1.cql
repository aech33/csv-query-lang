def prod A B := { x ++ y | x <- A, y <- B }

def main :=
  let A := removeEmpty (load 2 "A.csv") in
  let B := removeEmpty (load 2 "B.csv") in
  printCSV (sortCSV (prod A B))
