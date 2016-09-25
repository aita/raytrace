open Batteries

let nx = 200
let ny = 99

let () =
  Printf.printf "P3\n%d %d\n255\n" nx ny;
  for j = ny - 1 downto 0 do
    for i = 0 to nx - 1 do
      let r = float_of_int i /. float_of_int nx in
      let g = float_of_int j /. float_of_int ny in
      let b = 0.2 in
      let r = int_of_float (255.99 *. r) in
      let g = int_of_float (255.99 *. g) in
      let b = int_of_float (255.99 *. b) in
      Printf.printf "%d %d %d\n" r g b
    done
  done
  
