type xy = float * float (* x, y *)
type wh = float * float (* width x height *)

type sprite_params =
  {
    max_frames: int;
    img_src: string;
    frame_size: wh;
    src_offset: xy;
    bbox_offset: xy;
    bbox_size: wh;
  }

type sprite =
  {
    context: Dom_html.canvasRenderingContext2D Js.t;
    frame: int ref;
    max_frames: int;
    img: Dom_html.imageElement Js.t;
    frame_size: wh;
    src_offset: xy;
  }

val setup_sprite : string -> int -> wh -> xy -> sprite_params

val new_sprite : sprite_params -> Dom_html.canvasRenderingContext2D Js.t
   -> sprite
