-- Task 2: Permutation, Drop and Matching
-- Given a single input A:3, for each row a1,a2,a3, output a3,a1 if a1 and a2 are equal.
-- This requires selection (to filter rows where a1=a2) and projection (to output a3,a1).

project[col(3), col(1)](
  select[col(1) = col(2)](
    A
  )
)