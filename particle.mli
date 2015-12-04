open Actors
open Sprite

type xy = {
  mutable x: float;
  mutable y: float;
} 

type part_params = {
  sprite: Sprite.sprite;
  rot: float;
  lifetime: int;
}

type particle = {
  params: part_params;
  part_type: Actors.part_typ;
  pos:  xy;
  vel:  xy;
  acc:  xy;
  mutable kill: bool;
  mutable life: int;
}

val make : ?vel:float*float -> ?acc:float*float -> Actors.part_typ -> float*float -> Dom_html.canvasRenderingContext2D Js.t -> particle

val process : particle -> unit
