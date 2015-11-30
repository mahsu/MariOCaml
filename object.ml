open Sprite
open Actors

type xy = {
  mutable x: float;
  mutable y: float;
}


type aabb = {
  center: xy;
  half: xy;
}

type obj_params = {
  has_gravity: bool;
  max_speed: bool;
}

type obj = {
  params: obj_params;
  pos: xy;
  speed: float;
  vel: xy;
  jumping: bool;
  grounded: bool;
  dir: 1d_dir;
  invuln: int;
}

type collidable =
  | Player of player_typ * sprite * obj
  | Enemy of enemy_typ * sprite * obj
  | Item of item_typ * sprite * obj
  | Block of block_typ * sprite * obj

type noncollidable =
  (*| Dead of dead_type * sprite*)
  | Scenery of sprite * obj

let make_player = function
  | _ -> failwith "todo"


let make_item = function
  | Mushroom -> failwith "todo"
  | FireFlower -> failwith "todo"
  | Star -> failwith "todo"
  | Coin -> failwith "todo"

let make_enemy = function
  | Goomba -> failwith "todo"
  | GKoopa -> failwith "todo"
  | RKoopa -> failwith "todo"
  | GKoopaShell -> failwith "todo"
  | RKoopaShell -> failwith "todo"

let make_block = function
  | QBlock -> failwith "todo"
  | QBlockUsed -> failwith "todo"
  | Brick -> failwith "todo"
  | UnBBlock -> failwith "todo"

let make_type = function
  | SPlayer t -> make_player t 
  | SEnemy t -> make_enemy t
  | SItem t -> make_item t
  | SBlock t -> make_block t

let spawn spawnable context (posx, posy) =
  let spr = Sprite.make spawnable dir context in
  let params = make_type spawnable in 
  let obj = {
    params;
    pos = {x=posx; y=posy};
    vel = {x=0.0;y=0.0};
    speed = 0.0;
    jumping = false;
    grounded = false;
    dir = Left;
    invuln = 0;
  } in
  match spawnable with
  | SPlayer t -> Player(t,spr,obj)
  | SEnemy t -> Enemy(t,spr,obj)
  | SItem t -> Item(t,spr,obj)
  | SBlock t -> Block(t,spr,obj)

let get_sprite = function
  | Player (_,s,_) | Enemy (_,s, _) | Item (_,s, _) | Block (_,s, _)  -> s

let get_obj = function
  | Player (_,_,o) 
  | Enemy (_,_,o) | Item (_,_,o) | Block (_,_,o) -> o

let get_aabb obj  =
  let spr = ((get_sprite obj).params)  in
  let obj = get_obj obj in
  let (offx, offy) = spr.bbox_offset in
  let (box,boy) = (obj.pos.x+.offx,obj.pos.y+.offy) in
  let (sx,sy) = spr.bbox_size in
  {
    center = {x=(box+.sx)/.2.;y=(boy+.sy)/.2.};
    half = {x=sx/.2.;y=sy/.2.};
  }

let update_vel obj = failwith "todo"

let update_pos obj = failwith "todo"

let check_collision o1 o2 =
  let b1 = get_aabb o1 and b2 = get_aabb o2 in
  let o1 = get_obj o1 in
  let vx = (b1.center.x) -. (b2.center.x) in
  let vy = (b1.center.y) -. (b2.center.y) in
  let hwidths = (b1.half.x) +. (b2.half.x) in
  let hheights = (b1.half.y) +. (b2.half.y) in
  let ox = hwidths -. abs_float vx in
  let oy = hheights -. abs_float vy in
  if abs_float vx < hwidths && abs_float vy < hheights then begin
    if ox >= oy then begin
      if vy > 0. then (o1.pos.y <- o1.pos.y+.oy; Some Up) 
      else (o1.pos.y <- o1.pos.y -. oy; Some Down)
    end else begin
      if vx > 0. then (o1.pos.x <- o1.pos.x +.ox; Some Left)
      else (o1.pos.x <- o1.pos.x -. ox; Some Right)
    end
  end else None
  
let kill = function
  | _ -> []
