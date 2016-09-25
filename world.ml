open Batteries

let hit world ray tmin tmax =
  let rec loop world hit t =
    match world with
    | [] -> hit
    | shape :: xs ->
        let hit' = Shape.hit shape ray tmin t in
        match hit' with
          | None -> 
              loop xs hit t
          | Some h -> 
              loop xs hit' h.Hit.t 
  in
  loop world None tmax
