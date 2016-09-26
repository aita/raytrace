open Batteries
open Bigarray
open Vec3

module Pixbuf = struct
  type t = {
    width : int;
    height : int;
    pixels : (float, float32_elt, c_layout) Array2.t;
  }

  let make w h =
    let pixels = Array2.create float32 c_layout (w*3) h in
    {
      width = w;
      height = h;
      pixels = pixels;
    }

  let write_ppm { width=w; height=h; pixels=pixels; } =
    Printf.printf "P3\n%d %d\n255\n" w h;
  
    let print_color x y = 
      let byte x = int_of_float (255.99 *. x) in
      let r = byte pixels.{x*3, y} in
      let g = byte pixels.{x*3+1, y} in
      let b = byte pixels.{x*3+2, y} in
      Printf.printf "%d %d %d\n" r g b 
    in
  
    for j = h - 1 downto 0 do
      for i = 0 to w - 1 do
        print_color i j
      done
    done
end
    
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

  let pixbuf = Pixbuf.make nx ny in

  let world = [
    Shape.sphere (Vec3.make 0.0 0.0 (-1.0)) 0.5;
    Shape.sphere (Vec3.make 0.0 (-100.5) (-1.0)) 100.0;
  ]
  in

  let camera = Camera.make () in

  let sample i j = 
    let next () =
      let sx = Random.float 1.0 in
      let sy = Random.float 1.0 in
      let u = (float_of_int i +. sx) /. float_of_int nx in
      let v = (float_of_int j +. sy) /. float_of_int ny in
      let ray = Camera.ray camera u v in
      color ray world
    in
    let rec loop c = function
      | 0 -> c
      | _ as cnt -> 
          let c' = c +| next () in
          loop c' (cnt - 1) 
    in
    (loop Vec3.zero ns) /| float_of_int ns 
  in

  for j = 0 to ny - 1 do
    for i = 0 to nx - 1 do
      let pixels = pixbuf.Pixbuf.pixels in
      let color = sample i j in
      pixels.{i*3, j} <- color.(0);
      pixels.{i*3+1, j} <- color.(1);
      pixels.{i*3+2, j} <- color.(2)
    done
  done;

  Pixbuf.write_ppm pixbuf
  
