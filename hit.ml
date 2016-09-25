type t = {
  t : float;
  p : Vec3.t;
  normal : Vec3.t;
}

let make t p normal = {
  t = t;
  p = p;
  normal = normal;
}
