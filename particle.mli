open Actors
open Sprite

type part_params = {
  sprite: Sprite.sprite;
  rot: float;
  lifetime: int;
}

type particle = {
  params: part_params;
  part_type: Actors.part_typ;
  pos:  Actors.xy;
  vel:  Actors.xy;
  acc:  Actors.xy;
  mutable kill: bool;
  mutable life: int;
}

val make : ?vel:float*float -> ?acc:float*float -> Actors.part_typ -> float*float -> Dom_html.canvasRenderingContext2D Js.t -> particle

val make_score : int -> float*float -> Dom_html.canvasRenderingContext2D Js.t -> particle
val process : particle -> unit
