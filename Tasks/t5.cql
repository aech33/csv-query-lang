def main :=
  let P := load 4 "P.csv" in
  let Q := load 4 "Q.csv" in
  putCSV {
    match [p,q] with
    | [[p1,p2,p3,p4], [q1,q2,q3,q4]] =>
        if p1 == q1 then
          match {if get p i == "" then get q i else get p i | i <- [1,2,3]} with
          | [r2,r3,r4] => [p1,r2,r3,r4]
          end
        else []
    end
  | p <- P, q <- Q}
