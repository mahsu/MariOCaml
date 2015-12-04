open Object

type viewport = {
  pos: Object.xy;
  v_dim: Object.xy;
  m_dim: Object.xy;
}

val make : float*float -> float*float -> viewport

val calc_viewport_point : float -> float -> float -> float

val in_viewport : viewport -> Object.xy -> bool

val out_of_viewport_below : viewport -> float -> bool

val coord_to_viewport : viewport -> Object.xy -> Object.xy

val update : viewport -> Object.xy -> viewport
