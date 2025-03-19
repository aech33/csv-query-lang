-- Task 4: Copying and Constants
-- Given a single input A:1, for each row a1, output a1,foo,a1.
-- This requires projection with both column references and string constants.

project[col(1), "foo", col(1)](
  A
)