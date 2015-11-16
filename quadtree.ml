
let node_capacity = 25

type xy = {x of float; y of float}

type aabb = {
  center: xy
  half: xy
}

type 'a point  = aabb * 'a

(* Quadrants I-IV *)
type QuadTree = 
  | Node of aabb * subs
  | Leaf of aabb * 'a point list
and subs = {
  nw: QuadTree;
  ne: QuadTree;
  sw: QuadTree;
  se: QuadTree;

let create width height =
  let p = {x = width/2; y = height/2} in
  let bb = {
    center = p;
    half = p;
  } in
  Leaf(bb,[])

let size_of_node qtree =
  |Node(_,_,_,_,_) -> 0
  |Leaf(_,pts) -> List.length pts

let in_box box pt =
  box.center.x + box.half.x >= pt.x && box.center.x - box.half.x <= x
  && box.center.y + box.half.y >= pt.y && box.center.y - box.half.y <= y

let split qtree = function
  | Leaf(bb,pts) ->
      let half = {x=bb.half.x/2; y=bb.half.y/2} in
      let ne = {center={x=bb.center.x+bb.half.x/2; y=bb.center.y-bb.half.y/2};
                half;} in
      let nw = {center={x=bb.center.x-bb.half.x/2; y=bb.center.y-bb.half.y/2};
                half;} in
      let sw = {center={x=bb.center.x-bb.half.x/2; y=bb.center.y+bb.half.y/2};
                half;} in
      let se = {center={x=bb.center.x+bb.half.x/2; y=bb.center.y+bb.half.y/2};
                half;} in
      let insert_leaf box pts = 
        List.fold_left Leaf(box,[]) (fun x -> insert (fst x) (snd x)) pts in
      let subs = {
        ne = insert_leaf ne pts;
        nw = insert_leaf nw pts;
        sw = insert_leaf sw pts;
        se = insert_leaf se pts;
      } in
      Node(bb,subs)
  | _  -> failwith "Already split"

let rec insert qtree objbox thing = 
  let qtree = if size_of_node qtree = node_capacity then split qtree else qtree in
  match qtree with
  | Node(bb,subs) ->
      (* Nodes that are exactly on the center axis lines are put into NW or SE *)
      let nw = if in_box subs.nw {x=objbox.center.x-objbox.half.x; y=objbox.center.y-objbox.half.y}
      then insert subs.nw objbox thing else subs.nw in
      let ne = if in_box subs.ne {x=objbox.center.x+objbox.half.x; y=objbox.center.y-objbox.half.y}
      then insert subs.ne objbox thing else subs.ne in
      let sw = if in_box subs.sw {x=objbox.center.x-objbox.half.x; y=objbox.center.y+objbox.half.y}
      then insert subs.sw objbox thing else subs.sw in
      let se = if in_box subs.se {x=objbox.center.x+objbox.half.x; y=objbox.center.y+objbox.half.y}
      then insert subs.se objbox thing else subs.se in
      Node(bb,{ne;nw;sw;se})
  | Leaf(bb,pts) -> Leaf(bb,(fst pts,thing::pts))
      

  

