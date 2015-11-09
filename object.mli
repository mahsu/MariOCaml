open Sprite

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

type collidable =
  | Player of collidable_object
  | Monster of collidable_object
  | Item of collidable_object
  | BBlock of collidable_object
  | UnBBlock of collidable_object


val new_object : Sprite.sprite_params -> Dom_html.canvasRenderingContext2D Js.t
          -> xy -> collidable_object

