open Sprite

type xy = float * float
type vel = float * float

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
  | Monster of sprite * obj
  | Item of sprite * obj
  | BBlock of sprite * obj
  | UnBBlock of sprite * obj


val get_sprite : collidable -> Sprite.sprite

val new_object : Sprite.sprite_params -> Dom_html.canvasRenderingContext2D Js.t
          -> xy -> collidable_obj

val check_collision : collidable -> collidable -> direction
