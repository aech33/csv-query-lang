def apply f x y z := f x y z 


def append l1 l2 := match l1 with 
  | #x z :: y => f x y
  | [a,b,c] => l2
  end