open Actors
open Object
(*
* 0 -> Ground Object
* 1 -> Block object
* 2 -> Enemy object
* 3 -> Item object
**)
type obj_coord =  int * (float * float)
(*Set boundary variables*)
(*Max height Mario can jump*)
let max_jump = 3;;

(*Max distance Mario can jump*)
let max_dist = 3;;

(*Height of ground based on number of grids*)
let ground_height = 2;;

let block_prob = 6;;
let enemy_prob = 1;;

(*Canvas is 512 by 256 (w*h) -> 32 by 16 blocks
* Let the first generaed map just be of size 5 by 5 blocks *)

(*Check if the given location checkloc is already part of the list of locations
* in the location list.*)
let rec mem_loc checkloc loclist =
  match loclist with
  |[] -> false
  |h::t -> if (checkloc = (snd h)) then true
           else mem_loc checkloc t


(*Convert list of locations from blocksize to pixelsize*)
let rec convert_list lst =
  match lst with
  |[] -> []
  |(h::t) -> [(fst h, ((fst (snd h))*.16., (snd (snd h))*.16.))]@(convert_list t)

(*Generates a list of enemy coordinates
* 1 -> RKoopa
* 2 -> GKoopa
* 3 -> Goomba
* _ -> TBD *)
let generate_enemies () = []
(*
let choose_enemy = function
  |1->
  |2->
  |3->
  |_->

*)
let rec avoid_overlap lst currentLst =
  match lst with
  |[] -> []
  |h::t -> if(mem_loc (snd h) currentLst) then avoid_overlap t currentLst
           else [h]@(avoid_overlap t currentLst)
(*
* Chooses the form of the blocks to be places.
* |0 |1-> Plane of 3 blocks
* |2 |3 -> Cross of blocks
* |4-> Wall of 3 blocks
* |5 |6-> Single block
* |_ -> y u fail
* When calling this method, leave a 1 block gap from canvas size*)
let choose_block_pattern blockw blockh cbx cby prob =
  if(cbx > blockw || cby > blockh) then []
  else
    let block_typ = Random.int 2 in
    match prob with
    |0 |1 ->
      if(blockw -. cbx = 2.) then [(block_typ, (cbx, cby));
        (block_typ,(cbx +. 1., cby));(block_typ,(cbx +. 2., cby))]
      else if (blockw -. cbx = 1.) then [(block_typ,(cbx, cby));
        (block_typ,(cbx +. 1., cby))]
      else [(block_typ,(cbx, cby))]
    |2 |3 ->
      let yBlocks =
        if(blockh -. cby = 0.) then [(block_typ,(cbx, cby -. 1.))]
        else if (cby = 0.) then [(block_typ, (cbx, cby +. 1.))]
        else [(block_typ,(cbx, cby -. 1.));(block_typ,(cbx, cby +. 1.))] in
      let xBlocks =
        if(blockw -. cbx = 0.) then [(block_typ, (cbx -. 1., cby))]
        else if (cbx = 0.) then [(block_typ, (cbx +. 1., cby))]
        else [(block_typ, (cbx -. 1., cby));(block_typ,(cbx +. 1., cby))] in
      yBlocks@[(block_typ, (cbx, cby))]@xBlocks
    |4 ->
      if ((cby +. 3.) -. blockh = 2.) then [(block_typ,(cbx, cby))]
      else if ((cby +. 3.) -. blockh = 1.) then [(block_typ, (cbx,cby));
        (block_typ, (cbx, cby +. 1.))]
      else [(block_typ,(cbx, cby)); (block_typ,(cbx, cby +. 1.));
        (block_typ,(cbx, cby +. 2.))]
    |5 |6 -> [(0,(cbx, cby))]
    |_ -> failwith "Shouldn't reach here"

let rec generate_block_locs (blockw: float) (blockh: float) (cbx: float)
                    (cby: float) (acc: obj_coord list) =
  if(cbx > blockw) then acc
  else if (cby > (blockh-. 1.)) then
    generate_block_locs blockw blockh (cbx+.1.) 0. acc
  else if(mem_loc (cbx, cby) acc) then
    generate_block_locs blockw blockh cbx (cby+.1.) acc
  else
    let prob = Random.int 20 in
    let block_prob = Random.int 7 in
      if(prob <= block_prob) then
        let newacc = choose_block_pattern blockw blockh cbx cby prob in
        let undup_lst = avoid_overlap newacc acc in
        generate_block_locs blockw blockh cbx (cby+.1.) undup_lst
      else generate_block_locs blockw blockh cbx (cby+.1.) acc

(*Generates the list of brick locations needed to display the ground.
* 1/10 chance that a ground block is skipped each call.*)
let rec generate_ground (blockw:float) (blockh:float) (inc:float)
                        (acc: obj_coord list) =
  if(inc > blockw) then acc
  else
    let skip = Random.int 10 in
    let newacc = acc@[(1, (blockh *. 16.,inc))] in
    if (skip = 7) then generate_ground blockw blockh (inc +. 1.) acc
    else  generate_ground blockw blockh (inc +. 1.) newacc

let choose_sblock_typ typ =
  match typ with
  |0 -> Brick
  |1 -> UnBBlock
  |2 -> QBlock Mushroom
  |_ -> failwith "Shouldn't reach here"

let rec convert_to_block_obj lst context =
  match lst with
  |[] -> []
  |h::t ->
    let sblock_typ = choose_sblock_typ (fst h) in
    let ob = Object.spawn (SBlock sblock_typ) context (snd h) in
    [ob]@(convert_to_block_obj t context)

(*Procedurally generates a map given canvas width, height and context*)
let generate_helper blockw blockh cx cy context =
  let block_locs = generate_block_locs blockw blockh 0. 0. [] in
  let converted_block_locs = convert_list block_locs in
  let obj_converted_block_locs = convert_to_block_obj converted_block_locs context in
  obj_converted_block_locs


  (*  let brick1 = Object.spawn (SBlock Brick) context (200.0,200.0) in*)

let generate blockw blockh context =
  generate_helper blockw blockh 0. 0. context