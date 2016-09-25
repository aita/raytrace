open Vec3 

type t = {
  center : Vec3.t;
  radius : float;
}

let make c r = {
  center = c;
  radius = r;
}

let hit sphere ray tmin tmax = 
  let center = sphere.center in
  let r = sphere.radius in

  let hit_test t =
    if t < tmax && t > tmin then
      let p = Ray.point_at_parameter ray t in
      let normal = (p -| center) /| r in 
      Some (Hit.make t p normal)
    else
      None
  in

  let oc = Ray.origin ray -| center in
  let d = Ray.direction ray in
  let a = dot d d in
  let b = 2.0 *. dot oc d in
  let c = dot oc oc -. r *. r in
  let discritminant = b *. b -. 4.0 *. a *. c in
  if discritminant > 0. then
    let t, t' = 
      let a' = 2.0 *. a in
      let b' = -. b in
      let sd = sqrt discritminant in
      ((b' -. sd) /. a', (b' +. sd) /. a')
    in
    match hit_test t with
    | Some _ as h -> h
    | None -> hit_test t'
  else
    None
