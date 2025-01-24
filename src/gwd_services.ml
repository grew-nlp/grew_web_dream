open Dream_utils
open Printf
open Dep2pictlib
open Conll
open Grewlib

(* ================================================================================================ *)
type global_config = {
  conll: Conll_config.t;
  tf_wf: bool;
}

let current_config = ref {
    conll = Conll_config.build "sud";
    tf_wf = false;
  }

let filter () feature =
  match feature, !current_config.tf_wf with
  | ("wordform", false) -> false
  | ("textform", false) -> false
  | _ -> true

(* ================================================================================================ *)
type display =
  | Dep
  | Dot

type corpus = Corpus.t option

type history = (Deco.t * (string * int) * Deco.t * Graph.t) list

type state = {
  display: display;
  grs: Grs.t option;
  corpus: corpus;
  graph: Graph.t option;
  normal_forms: Graph.t list option;
  normal_form: Graph.t option;
  history: history option;
  position: int option;
}

let init_state = {
  display= Dep;
  grs= None;
  corpus= None;
  graph= None;
  normal_forms= None;
  normal_form= None;
  history= None;
  position=None;
}

let current = ref String_map.empty

let current_update session_id state_fct =
  match String_map.find_opt session_id !current with
  | None -> error "Unknown id `%s`" session_id
  | Some state -> current := String_map.add session_id (state_fct state) !current
let base_dir session_id = List.fold_left Filename.concat "" [Dream_config.get_string "extern"; "auto"; session_id]

let grs_dir session_id = List.fold_left Filename.concat "" [Dream_config.get_string "extern"; "auto"; session_id; "grs"]
let grs_url session_id = List.fold_left Filename.concat "" [Dream_config.get_string "base_url"; "auto"; session_id; "grs"]

let images_dir session_id = List.fold_left Filename.concat "" [Dream_config.get_string "extern"; "auto"; session_id; "images"]
let images_url session_id = List.fold_left Filename.concat "" [Dream_config.get_string "base_url"; "auto"; session_id; "images"]

let init_session () =
  let session_id = sprintf "%04x%04x%04x%04x" (Random.int 0xFFFF) (Random.int 0xFFFF) (Random.int 0xFFFF) (Random.int 0xFFFF) in
  current := String_map.add session_id init_state !current;
  FileUtil.mkdir ~parent:true (images_dir session_id);
  session_id

let connect () = `String (init_session ())

(* TODO: config must be client specific and not shared *)
let set_config _session_id json_config =
  let open Yojson.Basic.Util in
  try
    let assoc = json_config |> Yojson.Basic.from_string |> to_assoc in
    begin
      match List.assoc_opt "conll_config" assoc with
      | Some (`String c) -> current_config := {!current_config with conll = Conll_config.build c }
      | _ -> ()
    end;
    `Null
  with
  | Type_error _ -> error "Ill-formed config `%s`. It must be a JSON object" json_config
  | Yojson.Json_error _ -> error "`%s` config is not a valid JSON data" json_config


let meta_list_from_corpus corpus = 
  Corpus.fold_right 
    (fun sent_id graph acc -> 
       (sent_id, `Assoc (List.map (fun (f,v) -> (f, `String v)) (Graph.get_meta_list graph))) :: acc
    ) corpus []
  |> (fun x -> `Assoc x)

let uid () = Unix.gettimeofday () *. 10000. |> int_of_float |> string_of_int

let upload_corpus session_id file =
  let log_file = Filename.concat (base_dir session_id) ((uid ())^".log") in
  let corpus = Corpus.from_file ~log_file ~config:!current_config.conll file in

  let warn_list = 
    if Sys.file_exists log_file
    then 
      begin 
        let stream = Yojson.Basic.seq_from_file log_file in
        let acc = ref [] in
        Seq.iter (fun warn -> acc := warn :: !acc) stream;
        List.rev !acc
      end
    else [] in

  current_update session_id
    (fun state ->
       { state with corpus = Some corpus;
                    graph=None; normal_forms=None; normal_form=None; history=None; position=None;
       }
    );
  `Assoc [
    ("meta_list", meta_list_from_corpus corpus);
    ("warnings", (`List warn_list))
  ]


let dep_save ?deco session_id graph =
  let dep = Graph.to_dep ~filter:(filter ()) ?deco ~config:!current_config.conll graph in
  let d2p = Dep2pictlib.from_dep dep in
  let file = sprintf "%s.svg" (uid ()) in
  let filename = Filename.concat (images_dir session_id) file in
  Dep2pictlib.save_svg ~filename d2p;
  `String (Filename.concat (images_url session_id) file)

let dot_save ?deco session_id graph =
  let dot = Graph.to_dot ?deco ~config:!current_config.conll graph in
  let (temp_file_name,out_ch) =
    Filename.open_temp_file
      ~mode:[Open_rdonly;Open_wronly;Open_text] "grew_" ".dot" in
  fprintf out_ch "%s" dot;
  close_out out_ch;
  let file = sprintf "%s.svg" (uid ()) in
  let filename = Filename.concat (images_dir session_id) file in
  let command = sprintf "dot -Tsvg -o %s %s " filename temp_file_name in
  match Sys.command command with 
  | 0 -> `String (Filename.concat (images_url session_id) file)
  | n -> error "Fail to run (code=%d) command `%s`" n command


let graph_save ?deco session_id graph =
  let state = String_map.find session_id !current in
  match state.display with
  | Dep -> dep_save ?deco session_id graph
  | Dot -> dot_save ?deco session_id graph


let select_graph session_id sent_id =
  let state = String_map.find session_id !current in
  match state.corpus with
  | None -> error "No corpus loaded"
  | Some corpus ->
    match Corpus.graph_of_sent_id sent_id corpus with
    | None -> error "No sent_id: %s" sent_id
    | Some graph -> 
      (* let data = Conll_corpus.get_data corpus in
         match CCArray.find_map (fun (id,graph) -> if id=sent_id then Some graph else None) data with
         | None -> error ("No sent_id" ^ sent_id)
         | Some Conll ->
         Graph.of_json (Conll.to_json Conll) in *)
      current_update session_id
        (fun state ->
           { state with graph = Some graph; normal_forms=None; normal_form=None; history=None; position=None; }
        );
      graph_save session_id graph



(* 
(* Returns one `Assoc item with key "strategies" if possible else ("packages" if possible else "rules") *)
let exported_from_grs grs =
  match Grs.get_strat_list grs with
  | [] ->
    begin
      match Grs.get_package_list grs with
      | [] -> ("rules", `List (List.map (fun x -> `String x) (Grs.get_rule_list grs)))
      | packages -> ("packages", `List (List.map (fun x -> `String x) packages))
    end
  | strats -> ("strategies", `List (List.map (fun x -> `String x) strats))

let upload_grs session_id file =
  let _tmpfile = Eliom_request_info.get_tmp_filename file in
  let grs = Grs.load ~config:!current_config.conll _tmpfile in
  current_update session_id (fun state -> { state with grs = Some grs });
  `Assoc [exported_from_grs grs]

let upload_json_grs session_id json_file =
  let _tmpfile = Eliom_request_info.get_tmp_filename json_file in
  let json = Yojson.Basic.from_file _tmpfile in
  let grs = Grs.of_json ~config:!current_config.conll json in
  current_update session_id (fun state -> { state with grs = Some grs });
  `Assoc [exported_from_grs grs]

let upload_grs_code session_id code =
  let grs = Grs.parse ~config:!current_config.conll code in
  current_update session_id (fun state -> { state with grs = Some grs });
  `Assoc [exported_from_grs grs]

let url_grs session_id url =
  match Curly.(run (Request.make ~url ~meth:`GET ())) with
  | Error _ -> raise (Error (sprintf "Fail to load grs on URL `%s`" url))
  | Ok x ->
    match x.Curly.Response.code with
    | 200 ->
      let data = x.Curly.Response.body in
      let grs = Grs.parse ~config:!current_config.conll data in
      current_update session_id (fun state -> { state with grs = Some grs });
      `Assoc [
        ("code", `String data);
        exported_from_grs grs
      ]
    | 404 -> raise (Error (sprintf "URL not found `%s`" url))
    | code -> raise (Error (sprintf "Network error %d on URL `%s`" code url))

let upload_file session_id path file =
  let _tmpfile = Eliom_request_info.get_tmp_filename file in
  let (subpath, file) = split_path path in
  let dir = Filename.concat (grs_dir session_id) subpath in
  FileUtil.mkdir ~parent:true dir;
  let _ = FileUtil.cp [_tmpfile] (Filename.concat dir file) in
  `Null

let load_grs session_id grs_file =
  FileUtil.mkdir ~parent:true (grs_dir session_id);
  let grs = Grs.load ~config:!current_config.conll (Filename.concat (grs_dir session_id) grs_file) in
  current_update session_id (fun state -> { state with grs = Some grs });
  `Assoc [exported_from_grs grs]

let save file =
  let _tmpfile = Eliom_request_info.get_tmp_filename file in
  let filename = Eliom_request_info.get_original_filename file in
  let size = Eliom_request_info.get_filesize file |> Int64.to_string in
  `Assoc [("filename", `String filename); ("size", `String size) ]


let url_corpus session_id url =
  let ext = Filename.extension url in
  match Curly.(run (Request.make ~url ~meth:`GET ())) with
  | Error _ -> raise (Error (sprintf "Fail to load grs on URL `%s`" url))
  | Ok x ->
    match x.Curly.Response.code with
    | 200 ->
      let data = x.Curly.Response.body in
      let corpus = Corpus.from_string ~ext ~config:!current_config.conll data in 
      current_update session_id
        (fun state ->
           { state with corpus = Some corpus;
                        graph=None; normal_forms=None; normal_form=None; history=None; position=None;
           }
        );
      (meta_list_from_corpus corpus)
    | 404 -> raise (Error (sprintf "URL not found `%s`" url))
    | code -> raise (Error (sprintf "Network error %d on URL `%s`" code url))

let from_data ?conll ?json ?grs () =
  let session_id = init_session () in
  begin
    match conll with
    | None -> ()
    | Some c -> 
      let filename = Filename.concat (images_dir session_id) "corpus.conll" in
      let out_ch = open_out filename in
      Printf.fprintf out_ch "%s" c;
      close_out out_ch;
  end;
  begin
    match json with
    | None -> ()
    | Some j -> 
      let filename = Filename.concat (images_dir session_id) "corpus.json" in
      let out_ch = open_out filename in
      Printf.fprintf out_ch "%s" j;
      close_out out_ch;
  end;
  begin
    match grs with
    | None -> ()
    | Some code ->
        let grs = Grs.parse ~config:!current_config.conll code in
        current_update session_id (fun state -> { state with grs = Some grs })
  end;
  `Assoc [("session_id", `String session_id)]

let get_grs session_id =
  match String_map.find_opt session_id !current with
  | None -> raise (Error (sprintf "Unknown id `%s`" session_id))
  | Some { grs = None } -> `Null
  | Some { grs = Some g } -> `Assoc [exported_from_grs g]

let get_corpus session_id =
  let state = String_map.find session_id !current in
  match state.corpus with
  | None -> `Null
  | Some corpus ->
    `Assoc [
      ("meta_list", meta_list_from_corpus corpus);
      ("warnings", (`List []))
    ]



let rewrite session_id strat =
  let state = String_map.find session_id !current in
  match (state.graph, state.grs) with
  | (None, _) -> raise (Error "No graph selected")
  | (_, None) -> raise (Error "No GRS loaded")
  | (Some graph, Some grs) ->
    Grewlib.set_track_history true;
    let graph_list = Rewrite.simple_rewrite ~config:!current_config.conll graph grs strat in
    let (log : Yojson.Basic.t) = Rewrite.log_rewrite () in
    current_update session_id
      (fun state ->
         { state with normal_forms = Some graph_list; normal_form=None; history=None; position=None; }
      );
    `Assoc [
      ("normal_forms", `List (List.map (fun g -> `Int (Graph.trace_depth g)) graph_list));
      ("log", log)
    ]

let select_normal_form session_id position =
  let state = String_map.find session_id !current in
  match state.normal_forms with
  | None -> raise (Error "Inconsistent_state [normal_forms]")
  | Some nfs ->
    match List.nth_opt nfs (int_of_string position) with
    | None -> raise (Error "Inconsistent_state [position]")
    | Some graph ->
      current_update session_id
        (fun state ->
           { state with normal_form = Some graph; history=None; position=None; }
        );
      graph_save session_id graph

let save_normal_form session_id format =
  let state = String_map.find session_id !current in
  match (state.normal_form, format) with
  | (None, _) -> raise (Error "Inconsistent_state [normal_form]")
  | (Some nf, "json") ->
    let json = Graph.to_json nf in
    let file = sprintf "%s.json" (uid ()) in
    let filename = Filename.concat (images_dir session_id) file in
    Yojson.Basic.to_file filename json;
    `String (Filename.concat (images_url session_id) file)
  | (Some nf, "conll") ->
    let conll = nf |> Graph.to_json |> Conll.of_json |> Conll.to_string ~config:!current_config.conll in
    let file = sprintf "%s.conllu" (uid ()) in
    let filename = Filename.concat (images_dir session_id) file in
    CCIO.with_out filename (fun oc -> CCIO.write_line oc conll);
    `String (Filename.concat (images_url session_id) file)
  | (Some nf, f) -> 
    raise (Error ("Unknown format: " ^ format))

let rules session_id =
  let state = String_map.find session_id !current in
  match state.normal_form with
  | None -> raise (Error "No selected formal form")
  | Some graph ->
    let history = Graph.get_history graph in
    current_update session_id
      (fun state ->
         { state with history = Some history; position=None; }
      );
    let rules = List.map (fun (_,(r,l),_,_) -> `List [`String r; `Int l]) history in
    `List rules

(* return the assoc list of things to (re)draw *)
let draw_before_after session_id =
  let state = String_map.find session_id !current in
  match (state.history, state.position) with
  | (Some hist, Some pos) ->
    let ((graph_before, up_deco), (graph_after, down_deco))  =
      match CCList.drop pos hist with
      | [] -> raise (Error "No such item in history")
      | [(u,_,d,last)] ->
        (
          (last, u),
          ((match state.normal_form with Some g -> g | None -> raise (Error "Bug normal form")), d)
        )
      | (u,_,d,x)::(_,_,_,y)::_ -> ((x,u),(y,d)) in
    [
      ("before", graph_save ~deco:up_deco session_id graph_before);
      ("after", graph_save ~deco:down_deco session_id graph_after)
    ]
  | _ -> []

let select_rule session_id string_position =
  let position = int_of_string string_position in
  let state = String_map.find session_id !current in
  match state.history with
  | None -> raise (Error "No history")
  | Some hist ->
    current_update session_id (fun state -> { state with position = Some position });
    `Assoc (draw_before_after session_id)

let set_display session_id display =
  let state = String_map.find session_id !current in
  current_update session_id
    (fun state -> { state with display = if display = "graph" then Dot else Dep });
  `Assoc (
    draw_before_after session_id
    |> (fun l -> match state.graph with Some g -> ("init", graph_save session_id g) :: l | None -> l)
    |> (fun l -> match state.normal_form with Some g -> ("final", graph_save session_id g) :: l | None -> l)
  )


(* -----------------------------------------------------------------------*)
(* MAIN *)
(* -----------------------------------------------------------------------*)
let _ =

  (* let () = Ocsigen_config.set_maxrequestbodysizeinmemory 1_000_000_000 in *)

  try
    (* Read config *)
    let _ =
      let elements =
        List.map
          (fun item ->
             Ocsigen_extensions.Configuration.element
               ~name: item
               ~pcdata: (fun x -> printf " INFO:  ---> set `%s` config parameter to `%s`\n%!" item x; set_global item x)
               ()
          )
          ["log"; "base_url"; "extern"] in

      Ocsigen_extensions.Configuration.process_elements
        ~in_tag:"eliommodule"
        ~elements
        (Eliom_config.get_config ()) in

    Log.init()

  with
  | Error msg -> printf " ERROR: ================ Starting error: %s ================\n%!" msg; exit 0

 *)
