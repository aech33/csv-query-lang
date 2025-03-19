-- Task 5: Left merge on first column
-- Given two inputs P:4 and Q:4, for each pair of rows p1,p2,p3,p4 in P and q1,q2,q3,q4 in Q where p1=q1,
-- output p1,r2,r3,r4 where ri is qi if pi is empty and pi otherwise.
-- This requires a join on the first column and then a coalesce operation on the other columns.

project[
  col(1),
  coalesce(col(2), col(6)),
  coalesce(col(3), col(7)),
  coalesce(col(4), col(8))
](
  select[col(1) = col(5)](
    cartesian(A, B)
  )
)