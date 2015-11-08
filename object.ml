open Sprite

(* type obj_type = |Block |Monster |Player
type block = | Breakable | Unbreakable *)

type xy = float * float
type vel = float * float
type obj = {
  sprite: sprite;
  pos: xy;
}
type collidable_object = {
  sprite: sprite;
  pos: xy;
  speed: float;
  vel: vel;
  jumping: bool;
  grounded: bool;
}

type collidable = Player of collidable_object
  | Monster of collidable_object
  | Item of collidable_object
  | BBlock of collidable_object
  | UnBBlock of collidable_object

let new_object spr_param context pos_xy =
  let spr = new_sprite spr_param context in
  {
    sprite =  spr;
    pos = pos_xy;
  }
