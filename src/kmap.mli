module Render : sig
  val draw_kmap4 : int list list -> Vg.image
  val draw_kmap3 : int list list -> Vg.image
  val draw_kmap2 : int list list -> Vg.image
end

module Notebook : sig

  (* add the following to render the resulting kmaps (as SVG)
     into the iocaml notebook 

type svg_render = Gg.size2 * Gg.box2 * Vg.image

let string_of_svg ?(xml_decl=false) (size,view,image) = 
    let buffer = Buffer.create 1024 in
    let r = Vg.Vgr.create (Vgr_svg.target ~xml_decl ()) (`Buffer buffer) in
    ignore (Vg.Vgr.render r (`Image (size, view, image)));
    ignore (Vg.Vgr.render r `End);
    Buffer.contents buffer

let print_svg fmt svg = 
    Format.fprintf fmt "<svg>";
    Iocaml.display 
        "text/html" 
            ("<object data=" ^ Iocaml.data_uri "image/svg+xml" (string_of_svg svg) ^ "></object>")
;;
#install_printer print_svg;;

  *)

  type notebook_svg_render = Gg.size2 * Gg.box2 * Vg.image

  val draw_kmap4 : int list list -> notebook_svg_render
  val draw_kmap3 : int list list -> notebook_svg_render
  val draw_kmap2 : int list list -> notebook_svg_render

  val draw_kmaps4 : int list list array array -> notebook_svg_render
  val draw_kmaps3 : int list list array array -> notebook_svg_render
  val draw_kmaps2 : int list list array array -> notebook_svg_render

end

