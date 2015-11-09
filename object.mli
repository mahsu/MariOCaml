open Sprite

type xy = float * float
type vel = float * float


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


val new_object : Sprite.sprite_params -> Dom_html.canvasRenderingContext2D Js.t
          -> xy -> collidable_obj

