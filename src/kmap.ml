module Render = struct

  open Gg
  open Vg

  let font =
    { Font.name = "Open Sans"; size = 1.0; weight = `W400; slant = `Normal}
  let outline = `O { P.o with P.width = 0.02; (*P.dashes=Some (0.1,[0.05;0.1])*) }
  let black = I.const Color.black
  let white = I.const Color.white

  let utf8 =
    let b = Buffer.create 64 in
    (fun u ->
      Buffer.clear b;
      List.iter (fun u -> Uutf.Buffer.add_utf_8 b u) u;
      let str = Buffer.contents b in
      str)

  let string font p s i = 
    let text = Array.init (String.length s) 
      (fun i -> Uchar.of_int @@ Char.code (String.get s i)) |> Array.to_list |> utf8 
    in
    I.cut_glyphs ~text font [] i >> I.move p

  (* unit box *)
  let box = 
    I.cut ~area:outline 
      (P.rect (Box2.v (P2.v 0.0 0.0) (Size2.v 1. 1.)) P.empty) 
      (I.const Color.black)

  let box1 = box >> I.blend (string {font with Font.size=0.4} (P2.v 0.395 0.365) "1" black)

  let boxes d = 
    let y' = List.length d in
    snd @@ List.fold_left 
      (fun (y,img) b -> 
        let _, img = 
          List.fold_left 
            (fun (x,img) b ->
              (x+1), I.blend img @@ 
                I.move (V2.v (float_of_int x) (float_of_int y)) 
                (if b then box1 else box)) 
              (0,img) b
        in
        (y-1), img)
      (y'-1, I.void) d

  let pi = 4. *. atan 1.

  let outline_cubes = `O { P.o with P.width = 0.01; (*P.dashes=Some (0.113,[0.05;0.1])*) }
  let outline_col = Color.(v 0.7 0.2 0.2 1.)
  let shaded_col = Color.(v 0.7 0.2 0.2 0.1)

  (* pretty rendering of the covers *)
  let pc0 r = P.empty >> P.circle P2.(v 0. 0.) r

  let pc1 r = 
    P.empty >> P.sub P2.(v (-0.5) r) >> 
    P.line P2.(v 0. r) >> 
    P.earc ~cw:true Size2.(v r r) P2.(v 0. (-.r)) >> 
    P.line P2.(v (-0.5) (-.r))

  let pc2 r = 
    P.empty >> P.sub P2.(v (-0.5) r) >> 
    P.line P2.(v 0. r) >> 
    P.earc ~cw:true Size2.(v r r) P2.(v r 0.) >> 
    P.line P2.(v r (-0.5))

  let pc2c r = pc2 r >> P.line P2.(v (-0.5) (-0.5))

  let img ?(area=`Anz) col p = I.cut ~area p (I.const col) 

  let shaded0 r = 
    I.blend 
      (img ~area:outline_cubes outline_col (pc0 r)) 
      (img shaded_col (pc0 r))

  let shaded1 r = 
    I.blend 
      (img ~area:outline_cubes outline_col (pc1 r)) 
      (img shaded_col (pc1 r))

  let shaded2 r = 
    I.blend 
      (img ~area:outline_cubes outline_col (pc2 r)) 
      (img shaded_col (pc2c r))

  let shaded3 r =
    let pc3 = P.empty >> P.sub P2.(v (-. r) 0.5) >> P.line P2.(v (-. r) (-0.5)) in
    let pc3c = P.empty >> P.rect Box2.(v P2.(v (-. r) (-0.5)) Size2.(v (1. -. (0.5 -. r)) 1.)) in
    I.blend
      (img ~area:outline_cubes outline_col pc3)
      (img shaded_col pc3c)

  let shaded4 r =
    let pc4 = 
      P.empty >> P.sub P2.(v (-. r) 0.5) >> P.line P2.(v (-. r) (-0.5))
              >> P.sub P2.(v r 0.5) >> P.line P2.(v r (-0.5))
    in
    let pc4c = P.empty >> P.rect Box2.(v P2.(v (-. r) (-0.5)) Size2.(v (r +. r) 1.)) in
    I.blend
      (img ~area:outline_cubes outline_col pc4)
      (img shaded_col pc4c)

  let shaded5 =
    let pc5 = P.empty >> P.rect Box2.(v P2.(v (-0.5) (-0.5)) Size2.(v 1. 1.)) in
    img shaded_col pc5

  let draw_cube_elt rad t l b r = 
    match t, l, b, r with
    (* no edges *)
    | false, false, false, false -> shaded5
    (* 1 edge *)
    | true , false, false, false -> shaded3 rad >> I.rot (3. *. pi /. 2.)
    | false, true , false, false -> shaded3 rad 
    | false, false, true , false -> shaded3 rad >> I.rot (pi /. 2.)
    | false, false, false, true  -> shaded3 rad >> I.rot pi
    (* 2 edges; l+r, t+b *)
    | true , false, true , false -> shaded4 rad >> I.rot (pi /. 2.)
    | false, true , false, true  -> shaded4 rad
    (* 2 edges; corners *)
    | true , true , false, false -> shaded2 rad >> I.rot (pi /. 2.)
    | true , false, false, true  -> shaded2 rad
    | false, true , true , false -> shaded2 rad >> I.rot pi
    | false, false, true , true  -> shaded2 rad >> I.rot (3. *. pi /. 2.)
    (* 3 edges *)
    | false, true , true , true  -> shaded1 rad >> I.rot (3. *. pi /. 2.)
    | true , false, true , true  -> shaded1 rad
    | true , true , false, true  -> shaded1 rad >> I.rot (pi /. 2.)
    | true , true , true , false -> shaded1 rad >> I.rot pi
    (* 4 edges *)
    | true , true , true , true  -> shaded0 rad

  let draw_cube lim rad l t w h = 
    let rec loop_y y = 
      if y = t+h then I.void
      else
        let t = y=t and b = y=(t+h-1) in
        let rec loop_x x = 
          if x = l+w then loop_y (y+1)
          else
            let l = x=l and r = x=(l+w-1) in
            let x' = float_of_int (x mod 4) and y' = float_of_int (lim - 1 - (y mod 4)) in
            draw_cube_elt rad t l b r >> I.move P2.(v (0.5 +. x') (0.5 +. y')) >> 
              I.blend (loop_x (x+1))
        in
        loop_x l
    in
    loop_y t

  let labels4, labels3, labels2 = 
    
    let font = { font with Font.size=0.3 } in
    let tick x = P.empty >> P.sub (P2.v (-0.5) (x +. 0.5)) >> P.line (P2.v 0. x) in

    let labels1 = ["0"; "1"] in
    let labels2 = ["00"; "01"; "11"; "10"] in

    let labels_h l = 
      snd @@ List.fold_left (fun (i,img) label -> 
        let str = string font V2.(v (float_of_int i) 0.) label black in
        (i+1), I.blend img str) (0,I.void) l
    in

    let labels_v l = 
      let off = List.length l - 1 in
      snd @@ List.fold_left (fun (i,img) label ->  
        let i' = off - i in
        let str = string font V2.(v 0.0 (float_of_int i')) label black in
        (i+1), I.blend img str) (0,I.void) l
    in
    
    let labels h v x = 
      I.move (P2.v 0.3 (x +. 0.1)) (labels_h h) >> I.blend @@ 
      I.move (P2.v (-0.36) 0.38) (labels_v v) >> I.blend @@
      I.cut ~area:outline (tick x) black
    in

    labels labels2 labels2 4.,
    labels labels2 labels1 2.,
    labels labels1 labels1 2.

  let select2 x y terms =
    let contains = List.fold_left (fun a x -> a && List.mem x terms) true in
    if contains [-x;-y;] then 0
    else if contains [-x;y] then 1
    else if contains [x;y] then 2
    else if contains [x;-y] then 3
    else if contains [x] then 2
    else if contains [-x] then 0
    else if contains [y] then 1
    else if contains [-y] then 3
    else 0

  let select1 x terms = 
    if List.mem x terms then 1
    else if List.mem (-x) terms then 0
    else 0

  let count x terms = 
    let terms' = x @ (List.map (fun x -> -x) x) in
    List.fold_left (fun a x -> a + (if List.mem x terms' then 1 else 0)) 0 terms

  let draw_ones d (r,c,w,h) = 
    for y=r to r+h-1 do
      for x=c to c+w-1 do
        let x, y = x mod 4, y mod 4 in
        d.(y).(x) <- true
      done
    done

  let draw_all_ones rows cols cubes = 
    let d = Array.init rows (fun _ -> Array.init cols (fun _ -> false)) in
    let () = List.iter (draw_ones d) cubes in
    let d = Array.map Array.to_list d |> Array.to_list in
    d

  let select_cube4 terms = 
    let row, col = select2 3 4 terms, select2 1 2 terms in
    let crow, ccol = count [3;4] terms, count [1;2] terms in
    match List.length terms, crow, ccol with
    | 4, 2, 2 -> row, col, 1, 1
    | 3, 2, 1 -> row, col, 2, 1
    | 3, 1, 2 -> row, col, 1, 2
    | 2, 1, 1 -> row, col, 2, 2
    | 2, 2, 0 -> row, col, 4, 1
    | 2, 0, 2 -> row, col, 1, 4
    | 1, 1, 0 -> row, col, 4, 2
    | 1, 0, 1 -> row, col, 2, 4
    | 0, _, _ -> 0, 0, 4, 4
    | _ -> failwith "bad cube terms?"

  let select_cube3 terms = 
    let row, col = select1 3 terms, select2 1 2 terms in
    let crow, ccol = count [3] terms, count [1;2] terms in
    match List.length terms, crow, ccol with
    | 3, 1, 2 -> row, col, 1, 1
    | 2, 1, 1 -> row, col, 2, 1
    | 2, 0, 2 -> row, col, 1, 2
    | 1, 1, 0 -> row, col, 4, 1
    | 1, 0, 1 -> row, col, 2, 2
    | 0, _, _ -> 0, 0, 4, 2
    | _ -> failwith "bad cube terms?"

  let select_cube2 terms = 
    let row, col = select1 2 terms, select1 1 terms in
    let crow, ccol = count [2] terms, count [1] terms in
    match List.length terms, crow, ccol with
    | 2, 1, 1 -> row, col, 1, 1
    | 1, 1, 0 -> row, col, 2, 1
    | 1, 0, 1 -> row, col, 1, 2
    | 0, _, _ -> 0, 0, 2, 2
    | _ -> failwith "bad cube terms?"

  let draw_kmap' rows cols select labels terms =
    let cubes = List.map select terms in
    let img = List.fold_left (fun img (r,c,w,h) ->
      I.blend (draw_cube rows 0.45 c r w h) img) I.void cubes
    in
    let d = draw_all_ones rows cols cubes in
    let img = I.move P2.(v 0.75 0.25) (img >> I.blend (boxes d) >> I.blend labels) in
    img

  let draw_kmap2 = draw_kmap' 2 2 select_cube2 labels2
  let draw_kmap3 = draw_kmap' 2 4 select_cube3 labels3
  let draw_kmap4 = draw_kmap' 4 4 select_cube4 labels4

end

module Notebook = struct

  open Gg
  open Vg

  type notebook_svg_render = Gg.size2 * Gg.box2 * Vg.image

  let draw_kmaps x y f maps = 
    let x, y = x +. 1., y +. 1. in
    let draw_row maps = 
      let _,img = Array.fold_left (fun (i,img) terms ->
        let kmap = I.move (P2.v (float_of_int i *. x) 0.) (f terms) in
        i+1, I.blend img kmap) (0,I.void) maps
      in 
      img
    in
    let draw_rows maps = 
      let _, img = Array.fold_left (fun (i,img) maps ->
        let kmaps = I.move (P2.v 0. (float_of_int i *. (-. y))) (draw_row maps) in
        i+1, I.blend img kmaps) (1,I.void) maps
      in
      img
    in
    let length = Array.length maps in
    let img = draw_rows maps in
    I.move (P2.v 0. (float_of_int length *. y)) img

  let draw_kmap' x y f t = 
    Size2.v (x *. 20.) (y *. 20.), 
    Box2.v (P2.v (0.) (0.)) (Size2.v (x +. 1.) (y +. 1.)), f t

  let draw_kmap4 = draw_kmap' 4. 4. Render.draw_kmap4
  let draw_kmap3 = draw_kmap' 4. 2. Render.draw_kmap3
  let draw_kmap2 = draw_kmap' 2. 2. Render.draw_kmap2

  let draw_kmaps' x y f t = 
    let rows = float_of_int @@ Array.length t in
    let cols = float_of_int @@ Array.fold_left (fun x a -> max x (Array.length a)) 0 t in
    Size2.v (cols *. (x +. 1.) *. 10.) (rows *. (y +. 1.) *. 10.), 
    Box2.v (P2.v (0.) (0.)) (Size2.v (cols *. (x +. 1.)) (rows *. (y +. 1.))), 
    draw_kmaps x y f t

  let draw_kmaps4 = draw_kmaps' 4. 4. Render.draw_kmap4
  let draw_kmaps3 = draw_kmaps' 4. 2. Render.draw_kmap3
  let draw_kmaps2 = draw_kmaps' 2. 2. Render.draw_kmap2

end

