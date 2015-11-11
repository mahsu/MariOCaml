open Sprite
open Characters

type xy = float * float

type direction = | North | South | East | West

type obj_template = 
  {
    
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


val get_sprite : collidable -> Sprite.sprite

val spawn : Character.actor  -> Dom_html.canvasRenderingContext2D Js.t
          -> xy -> collidable
val kill : obj -> noncollidable list option
val update_vel : obj -> obj
val update_pos : obj -> obj
val check_collision : collidable -> collidable -> direction option

