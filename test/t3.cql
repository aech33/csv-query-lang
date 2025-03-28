-- Task 3: Existence Check
-- Given a single input A:2, for each row a1,a2, output a1,a2 if a2 is not the empty string.
-- This requires filtering rows where the second column is not empty.

project[col(1), col(2)](
  select[col(2) != ""](
    A
  )
)