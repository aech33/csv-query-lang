def main :=
  putCSV {
    match row with 
    | [a1] => [a1, "foo", a1]
    end
  | row <- load 1 "A.csv"}