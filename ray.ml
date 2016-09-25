type t = Vec3.t * Vec3.t

let make orig dir = (orig, dir)

let origin = fst
let direction = snd

let point_at_parameter ray t = 
  let o, d = ray in
  Vec3.(o +| t *| d)
