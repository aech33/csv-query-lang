def main :=
  putCSV {
    match row with
    | [a1, a2] => if a2 == "" then [] else row
    end
  | row <- removeEmpty (load 2 "A.csv") }
