open Batteries
open Vec3

let hit_sphere center radius ray =
  let oc = Ray.origin ray -| center in
  let d = Ray.direction ray in
  let a = dot d d in
  let b = 2.0 *. dot oc d in
  let c = dot oc oc -. radius *. radius in
  let discriminant = b *. b -. 4. *. a *. c in
  if discriminant < 0. then
    -1.0
  else
    ((-. b) -. sqrt discriminant) /. (2.0 *. a)

let color ray = 
  let t = hit_sphere (Vec3.make 0.0 0.0 (-1.0)) 0.5 ray in
  if t > 0.0 then
    let n = Ray.point_at_parameter ray t -| Vec3.make 0.0 0.0 (-1.0) in
    let n' = unit_vector n in
    0.5 *| (n' +| Vec3.one)
  else
    let unit_direction = unit_vector @@ Ray.direction ray in
    let t = 0.5 *. (unit_direction.(1) +. 1.0) in 
    let a = (1.0 -. t) *| Vec3.one in
    let b = t *| Vec3.make 0.5 0.7 1.0 in
    a +| b

let () =
  let nx = 200 in
  let ny = 100 in

  let print_color c = 
    let f x = int_of_float (255.99 *. x) in
    let r = f @@ c.(0) in
    let g = f @@ c.(1) in
    let b = f @@ c.(2) in
    Printf.printf "%d %d %d\n" r g b 
  in

  let lower_left_corner = Vec3.make (-2.0) (-1.0) (-1.0) in
  let horizontal = Vec3.make 4.0 0.0 0.0 in
  let vertical = Vec3.make 0.0 2.0 0.0 in
  let origin = Vec3.zero in

  Printf.printf "P3\n%d %d\n255\n" nx ny;
  for j = ny - 1 downto 0 do
    for i = 0 to nx - 1 do
      let u = float_of_int i /. float_of_int nx in
      let v = float_of_int j /. float_of_int ny in

      let direction = 
        lower_left_corner +| u *| horizontal +| v *| vertical in
      let ray = Ray.make origin direction in

      let col = color ray in
      print_color col
    done
  done
  
