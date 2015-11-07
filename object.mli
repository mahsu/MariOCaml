open Sprite

type obj_type = |Block |Monster |Player
type block =  | Breakable | Unbreakable

type xy = float * float
type obj = {
  sprite: Sprite.sprite;
  pos: xy;
}

val new_object : Sprite.sprite_params -> Dom_html.canvasRenderingContext2D Js.t
          -> xy -> obj

