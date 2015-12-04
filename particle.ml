open Actors
open Sprite

type xy = {
  mutable x: float;
  mutable y: float;
} 

type part_params = {
  sprite: Sprite.sprite;
  rot: float;
  lifetime: int;
}

type particle = {
  params: part_params;
  part_type: Actors.part_typ;
  pos:  xy;
  vel:  xy;
  acc:  xy;
  mutable kill: bool;
  mutable life: int;
}

let pair_to_xy pair = {
  x = fst pair;
  y = snd pair;
}

let make_params sprite rot lifetime = 
  {
    sprite;
    rot;
    lifetime;
  }

let make_type typ ctx =
  match typ with
  | GoombaSquish as t -> make_params (Sprite.make_particle t ctx) 0. 120
  | BrickChunkL as t -> make_params (Sprite.make_particle t ctx) 0. 300
  | BrickChunkR as t -> make_params (Sprite.make_particle t ctx) 0. 300

let make ?vel:(vel=(0.,0.)) ?acc:(acc=(0.,0.)) part_type pos ctx =
  let params = make_type part_type ctx in
  let pos = pair_to_xy pos and vel = pair_to_xy vel and acc = pair_to_xy acc in
  {
    params;
    part_type;
    pos;
    vel;
    acc;
    kill = false;
    life = params.lifetime;
  }
   
let update_vel part =
  part.vel.x <- (part.vel.x +. part.acc.x);
  part.vel.y <- (part.vel.y +. part.acc.y) 

let update_pos part =
  part.pos.x <- (part.vel.x +. part.pos.x);
  part.pos.y <- (part.vel.y +. part.pos.y)
  
let process_particle part =
  part.life <- part.life - 1;
  update_vel part;
  update_pos part 
