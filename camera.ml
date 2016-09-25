open Vec3

type t = {
  origin : Vec3.t;
  lower_left_corner : Vec3.t;
  horizontal : Vec3.t;
  vertical : Vec3.t;
}


let make () =
  let lower_left_corner = Vec3.make (-2.0) (-1.0) (-1.0) in
  let horizontal = Vec3.make 4.0 0.0 0.0 in
  let vertical = Vec3.make 0.0 2.0 0.0 in
  let origin = Vec3.zero in
  {
    lower_left_corner = lower_left_corner;
    horizontal = horizontal;
    vertical = vertical;
    origin = origin;
  }

let ray cam u v =
  let u' = u *| cam.horizontal in
  let v' = v *| cam.vertical in
  let target =
    cam.lower_left_corner +| u' +| v' -| cam.origin
  in
  Ray.make cam.origin target

    
