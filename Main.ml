open Core
open Parser

let () =
  match Sys.argv with
  | [| _; file |] ->    
    let ic = In_channel.create file in
    let lines = read_file ic in
    let samples = List.filter ~f:valid_sample lines in
    let column_count = column_count samples in
    print 
      (samples
       |> List.map 
         ~f:(fun s -> (Array.of_list (Str.split (Str.regexp "\t") s)).(5))
       |> slice column_count
      )
  | _ -> ()
;;