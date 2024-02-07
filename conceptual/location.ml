(* -- Use this in your solution without modifications *)
module PBox = PrintBox

type location = {start_pos : Lexing.position; end_pos : Lexing.position}

let make_location (startp, endp) = {start_pos = startp; end_pos = endp}

let dummy_loc = {start_pos = Lexing.dummy_pos; end_pos = Lexing.dummy_pos}


let location_style = PBox.Style.fg_color PBox.Style.Cyan

let location_to_tree ?(includefile = true) {start_pos; end_pos} =
  if includefile then
    if start_pos.pos_lnum = end_pos.pos_lnum then
      PBox.sprintf_with_style location_style "\"%s\" @ %d:%d-%d"
        start_pos.pos_fname start_pos.pos_lnum 
        (start_pos.pos_cnum - start_pos.pos_bol)
        (end_pos.pos_cnum - end_pos.pos_bol)
    else
      PBox.sprintf_with_style location_style "\"%s\" @ %d:%d-%d:%d"
        start_pos.pos_fname start_pos.pos_lnum 
        (start_pos.pos_cnum - start_pos.pos_bol)
        end_pos.pos_lnum 
        (end_pos.pos_cnum - end_pos.pos_bol)
  else
    if start_pos.pos_lnum = end_pos.pos_lnum then
      PBox.sprintf_with_style location_style "@ %d:%d-%d"
        start_pos.pos_lnum 
        (start_pos.pos_cnum - start_pos.pos_bol)
        (end_pos.pos_cnum - end_pos.pos_bol)
    else
      PBox.sprintf_with_style location_style "@ %d:%d-%d:%d"
        start_pos.pos_lnum 
        (start_pos.pos_cnum - start_pos.pos_bol)
        end_pos.pos_lnum 
        (end_pos.pos_cnum - end_pos.pos_bol)

let print_location loc = location_to_tree loc |> PrintBox_text.output stdout; print_endline "";