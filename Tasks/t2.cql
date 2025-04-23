def main :=
  let A := removeEmpty (load 3 "A.csv") in
  let output :=
        { match row with
          | [a1,a2,a3] => if a1 == a2 then [a3,a1] else []
          end
        | row <- A } in
  putCSV output
