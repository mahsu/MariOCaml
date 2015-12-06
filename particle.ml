open Actors
open Sprite

type part_params = {
  sprite: Sprite.sprite;
  rot: float;
  lifetime: int;
}

type particle = {
  params: part_params;
  part_type: Actors.part_typ;
  pos:  Actors.xy;
  vel:  Actors.xy;
  acc:  Actors.xy;
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
  | GoombaSquish as t -> make_params (Sprite.make_particle t ctx) 0. 30
  | BrickChunkL as t -> make_params (Sprite.make_particle t ctx) 0. 300
  | BrickChunkR as t -> make_params (Sprite.make_particle t ctx) 0. 300
  | Score100 as t -> make_params (Sprite.make_particle t ctx) 0. 30
  | Score200 as t -> make_params (Sprite.make_particle t ctx) 0. 30
  | Score400 as t -> make_params (Sprite.make_particle t ctx) 0. 30
  | Score800 as t -> make_params (Sprite.make_particle t ctx) 0. 30
  | Score1000 as t -> make_params (Sprite.make_particle t ctx) 0. 30
  | Score2000 as t -> make_params (Sprite.make_particle t ctx) 0. 30
  | Score4000 as t -> make_params (Sprite.make_particle t ctx) 0. 30
  | Score8000 as t -> make_params (Sprite.make_particle t ctx) 0. 30
  
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
   
let make_score score pos ctx = 
  let t = match score with
  | 100 -> Score100
  | 200 -> Score200
  | 400 -> Score400
  | 800 -> Score800
  | 1000 -> Score1000
  | 2000 -> Score2000
  | 4000 -> Score4000
  | 8000 -> Score8000
  | _ -> Score100
  in make ~vel:(0.5,-0.7) t pos ctx

let update_vel part =
  part.vel.x <- (part.vel.x +. part.acc.x);
  part.vel.y <- (part.vel.y +. part.acc.y) 

let update_pos part =
  part.pos.x <- (part.vel.x +. part.pos.x);
  part.pos.y <- (part.vel.y +. part.pos.y)
  
let process part =
  part.life <- part.life - 1;
  if part.life = 0 then (part.kill <- true);
  update_vel part;
  update_pos part 
