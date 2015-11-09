open Sprite


type xy = float * float
type vel = float * float
type destroyed = bool

type collidable_obj = {
  sprite: sprite;
  pos: xy;
  speed: float;
  vel: vel;
  jumping: bool;
  grounded: bool;
}

type collidable =
  | Player of collidable_obj
  | Monster of collidable_obj
  | Item of collidable_obj
  | BBlock of collidable_obj
  | UnBBlock of collidable_obj

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
