open Graphics ;;
open List ;;
(* images are lists of lists of floats between 0. (white) and 1. (black) *)
type image = float list list ;;
type size = int * int ;;

(* show the image *)
let depict (img : image) : unit =
  open_graph "";
  let cols, rows = length (hd img), length img in resize_window cols rows;
  let depict_pix change cur_row cur_col =
    let lvl = int_of_float (255. *. (1. -. change)) in
    set_color (rgb lvl lvl lvl);
    plot cur_col (rows - cur_row) in
  iteri (fun r_num row ->
           iteri (fun c_num pix -> depict_pix pix r_num c_num) row) img;
  Unix.sleep 2;
  close_graph () ;;

(* threshold thershold image -- image where pixels above the threshold value are black *)
let threshold (img : image) (t : float) : image =
  let map_rows = map (fun pix ->
                          if pix <= t
                          then 0.
                          else 1.) in
    map map_rows img ;;

(* dither max image -- dithered image *)
let dither (img : image) : image =
  let dither_rows = map (fun pix ->
                              if pix > Random.float 1.
                              then 1.
                              else 0.) in
  map dither_rows img ;;

let mona = Monalisa.image ;;
depict mona ;;
depict (threshold mona 0.75) ;;
depict (dither mona);;
