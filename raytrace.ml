open Batteries
open Vec3

let color ray world = 
  let hit = World.hit world ray 0.0 infinity in
  match hit with
  | Some h ->
      0.5 *| (h.Hit.normal +| Vec3.one)
  | None ->
      let unit_direction = unit_vector @@ Ray.direction ray in
      let t = 0.5 *. (unit_direction.(1) +. 1.0) in 
      let a = (1.0 -. t) *| Vec3.one in
      let b = t *| Vec3.make 0.5 0.7 1.0 in
      a +| b

let () =
  let nx = 200 in
  let ny = 100 in
  let ns = 100 in

  Printf.printf "P3\n%d %d\n255\n" nx ny;

  let print_color c = 
    let f x = int_of_float (255.99 *. x) in
    let r = f @@ c.(0) in
    let g = f @@ c.(1) in
    let b = f @@ c.(2) in
    Printf.printf "%d %d %d\n" r g b 
  in

  let world = [
    Shape.sphere (Vec3.make 0.0 0.0 (-1.0)) 0.5;
    Shape.sphere (Vec3.make 0.0 (-100.5) (-1.0)) 100.0;
  ]
  in

  let camera = Camera.make () in

  let sample i j = 
    let sx = Random.float 1.0 in
    let sy = Random.float 1.0 in
    let u = (float_of_int i +. sx) /. float_of_int nx in
    let v = (float_of_int j +. sy) /. float_of_int ny in
    let ray = Camera.ray camera u v in
    color ray world
  in

  for j = ny - 1 downto 0 do
    for i = 0 to nx - 1 do
      let rec loop c = function
        | 0 -> c
        | _ as cnt -> 
            let c' = c +| (sample i j) in
            loop c' (cnt - 1) 
      in
      let color = loop Vec3.zero ns in
      print_color (color /| float_of_int ns)
    done
  done
  
