open Sprite
open Actors

type xy = float * float

type direction = | Up | Down | Right | Left

type aabb = {
  center: xy;
  halfDimension;
}

type obj = {
  sprite: sprite;
  pos: xy;
  speed: float;
  vel: xy;
  jumping: bool;
  grounded: bool;
  dir: direction;
}

type collidable =
  | Player of actor * sprite * obj
  | Monster of actor * sprite * obj
  | Item of actor * sprite * obj
  | Block of actor * sprite * obj

type noncollidable =
  | Dead of dead_type * sprite
  | Scenery of sprite * obj

let new_object spr_param context pos_xy =
  let spr = new_sprite spr_param context in
  {
    sprite =  spr;
    pos = pos_xy;
    vel = (0.0,0.0);
    speed = 0.0;
    jumping = false;
    grounded = false;
  }


let get_sprite obj = match obj with
  | Player (s,_) | Monster (s, _) | Item (s, _) | BBlock (s, _) 
  | UnBBlock (s, _) -> s

let get_bbox_origin = failwith "todo"

let get_bbox_center = failwith "todo"
