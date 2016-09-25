type t =
  | Sphere of Sphere.t

let sphere center radius = Sphere (Sphere.make center radius)

let hit = function
  | Sphere sphere -> Sphere.hit sphere
