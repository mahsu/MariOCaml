open Actors

(* Represents an xy vector *)
type xy = float * float (* x, y *)

(* The type of animation of the sprite *)
type animation_typ = | Reflect | Frame

(* Inherent sprite parameters from which to create the sprite *)
type sprite_params =
  {
    max_frames: int;
    img_src: string;
    frame_size: xy;
    src_offset: xy;
    bbox_offset: xy;
    bbox_size: xy;
    anim: animation_typ;
  }

(* Concrete sprite created to visually represent an object *)
type sprite = 
  {
    context: Dom_html.canvasRenderingContext2D Js.t; 
    frame: int ref;
    max_frames: int;
    img: Dom_html.imageElement Js.t;
    frame_size: xy;
    src_offset: xy;
    bbox_offset: xy;
    bbox_size: xy;
    anim: animation_typ;
    x_refl: int;
  }


(* Sets up a sprite to create *)
val setup_sprite : ?anim:animation_typ -> string -> int -> xy -> xy 
                          -> sprite_params 

(* Creates a sprite given the actor type *)
val new_sprite : actor -> Dom_html.canvasRenderingContext2D Js.t
   -> sprite

(* Updates the sprite's animation *)
val update_animation : sprite -> unit

