
let rec to_string sep f = function
  | [] -> ""
  | [e] -> f e
  | e :: l -> (f e) ^ sep ^ (to_string sep f l)
