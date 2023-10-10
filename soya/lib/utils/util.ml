let rec get_k_first k l =
  if k = 0
  then []
  else
    match l with
    | [] -> []
    | r :: l -> r :: get_k_first (k-1) l

