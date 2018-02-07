open Core

let is_sample_line l = Str.string_match (Str.regexp "^[0-9]") l 0

let sample_row sample = 
  let columns = Str.split (Str.regexp "[ \t]") sample in
  match columns with
  | well :: sample :: tl -> Str.first_chars sample 1
  | _ -> ""

let rec get_column_count samples = 
  match samples with
  | [] -> 0
  | [hd] -> 1
  | hd1 :: hd2 :: tl -> 
    let row1 = sample_row hd1 in
    let row2 = sample_row hd2 in
    if row1 = row2
    then 1 + get_column_count (hd1 :: tl)
    else 1

let slice columns values =
  let f acc value =
    match acc with
    | [] -> [[value]]
    | hd :: tl when List.length hd = columns -> [value] :: hd :: tl
    | hd :: tl -> (value :: hd) :: tl
  in
  List.fold ~init:[] ~f:f (List.rev values)

let read_file ic =
  let rec read_file' file =    
    match In_channel.input_line file with
    | Some(l) -> l :: read_file' file
    | None -> [""]
  in
  read_file' ic

let rec print matrix =   
  match matrix with
  | [hd] :: tl ->
    printf "%s\n" hd;
    print tl
  | (hd :: tl) :: acc_tl ->
    printf "%s\t" hd;
    print (tl :: acc_tl)
  | [] | _ -> ()

let () =
  match Sys.argv with
  | [| _; file |] ->    
    let ic = In_channel.create file in
    let lines = read_file ic in
    let samples = List.filter ~f:is_sample_line lines in
    let column_count = get_column_count samples in
    print 
      (samples
       |> List.map 
         ~f:(fun s -> (Array.of_list (Str.split (Str.regexp "\t") s)).(5))
       |> slice column_count
      )
  | _ -> printf "Wrong number of arguments"
;;