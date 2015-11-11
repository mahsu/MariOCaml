open Sprite
open Characters

type xy = float * float
type vel = float * float
type destroyed = bool
type direction = | North | South | East | West

type collidable_obj = {
  sprite: sprite;
  pos: xy;
  speed: float;
  vel: vel;
  jumping: bool;
  grounded: bool;
}

type collidable =
  | Player of sprite * obj
  | Enemy of enemy_typ * sprite * obj
  | Item of item_typ * sprite * obj
  | Block of block_typ * sprite * obj

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
