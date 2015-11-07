open Sprite

type obj_type = |Block |Monster |Player
type block = | Breakable | Unbreakable

type xy = float * float
type obj = {
  sprite: sprite;
  pos: xy;
} 

let new_object spr_param context pos_xy = 
  let spr = new_sprite spr_param context in 
  {
    sprite =  spr;
    pos = pos_xy;
  }
