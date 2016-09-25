open Batteries

type t = float array

let make x y z = [| x; y; z; |]

let zero = [| 0.; 0.; 0.; |]
let one = [| 1.; 1.; 1.; |]

let neg v = [| -. v.(0); -. v.(1); -. v.(2); |]

let ( *| ) s v = [| s *. v.(0); s *. v.(1); s *. v.(2); |]
let ( +| ) a b = [| a.(0) +. b.(0); a.(1) +. b.(1); a.(2) +. b.(2); |]
let ( -| ) a b = [| a.(0) -. b.(0); a.(1) -. b.(1); a.(2) -. b.(2); |]

let ( /| ) v s = [| v.(0) /. s; v.(1) /. s; v.(2) /. s; |]

let dot a b = a.(0) *. b.(0) +. a.(1) *. b.(1) +. a.(2) *. b.(2)

let square_length v = dot v v
let length v = sqrt @@ square_length v

let unit_vector v = 
  let len = length v in
  Array.map (fun x -> x /. len) v
