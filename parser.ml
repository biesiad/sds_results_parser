open Core

let valid_sample sample =
  let columns = Str.split (Str.regexp "[ \t]") sample in
  match columns with
  | w :: s :: tl -> 
    (Str.string_match (Str.regexp "^[0-9]+$") w 0) && (Str.string_match (Str.regexp "^[A-Z][0-9]+$") s 0)
  | _ -> false

let sample_row sample = 
  let columns = Str.split (Str.regexp "[ \t]") sample in
  match columns with
  | w :: s :: tl -> Str.first_chars s 1
  | _ -> failwith "Not a valid sample row"

let rec column_count samples = 
  match samples with
  | [] -> 0
  | [hd] -> 1
  | hd1 :: hd2 :: tl -> 
    let row1 = sample_row hd1 in
    let row2 = sample_row hd2 in
    if row1 = row2
    then 1 + column_count (hd1 :: tl)
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
  | _ -> ()
