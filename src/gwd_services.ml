open Dream_utils
open Printf
open Dep2pictlib
open Conll
open Grewlib

(* ================================================================================================ *)

let filename_concat_list = List.fold_left Filename.concat "" 

let uid () = Unix.gettimeofday () *. 10000. |> int_of_float |> string_of_int

(* ================================================================================================ *)
module Session = struct
  type display = Dep | Dot

  type history = (Deco.t * (string * int) * Deco.t * Graph.t) list

  type t = {
    conll_config: Conll_config.t;
    tf_wf: bool;
    display: display;
    grs: Grs.t option;
    corpus: Corpus.t option;
    graph: Graph.t option;
    normal_forms: Graph.t list option;
    normal_form: Graph.t option;
    history: history option;
    position: int option;
  }

  let init = {
    conll_config = Conll_config.build "sud";
    tf_wf = false;
    display = Dep;
    grs = None;
    corpus = None;
    graph = None;
    normal_forms = None;
    normal_form = None;
    history = None;
    position =None;
  }

  let filter session feature =
    match feature, session.tf_wf with
    | ("wordform", false) -> false
    | ("textform", false) -> false
    | _ -> true

  let (current : t String_map.t ref) = ref String_map.empty
  let get session_id =
    match String_map.find_opt session_id !current with
    | Some session -> session
    | None -> error "Unknown id `%s`" session_id

  let update session_id state_fct =
    let session = get session_id in
    current := String_map.add session_id (state_fct session) !current
  
  let add session_id =
    current := String_map.add session_id init !current
end

(* ================================================================================================ *)

let base_dir session_id = filename_concat_list [Dream_config.get_string "extern"; "auto"; session_id]

let grs_dir session_id = filename_concat_list [Dream_config.get_string "extern"; "auto"; session_id; "grs"]
let grs_url session_id = filename_concat_list [Dream_config.get_string "base_url"; "auto"; session_id; "grs"]

let images_dir session_id = filename_concat_list [Dream_config.get_string "extern"; "auto"; session_id; "images"]
let images_url session_id = filename_concat_list [Dream_config.get_string "base_url"; "auto"; session_id; "images"]

let connect () =
  let session_id = sprintf "%04x%04x%04x%04x" (Random.int 0xFFFF) (Random.int 0xFFFF) (Random.int 0xFFFF) (Random.int 0xFFFF) in
  Session.add session_id;
  FileUtil.mkdir ~parent:true (images_dir session_id);
  `String session_id

(* TODO: unused, need an interface for this service *)
let set_config session_id json_config =
  let open Yojson.Basic.Util in
  try
    let assoc = json_config |> Yojson.Basic.from_string |> to_assoc in
    begin
      match List.assoc_opt "conll_config" assoc with
      | Some (`String c) -> 
        Session.update session_id (fun session -> {session with conll_config = Conll_config.build c})
      | _ -> ()
    end;
    `Null
  with
  | Type_error _ -> error "Ill-formed config `%s`. It must be a JSON object" json_config
  | Yojson.Json_error _ -> error "`%s` config is not a valid JSON data" json_config


let meta_list_from_corpus corpus =
  let counter =
    Corpus.fold_right 
      (fun sent_id _ acc ->
        let old_count = String_map.find_opt sent_id acc |> CCOption.get_or ~default: 0 in
        String_map.add sent_id (old_count + 1) acc
      ) corpus String_map.empty in
  let amb_counter = String_map.filter (fun _ n -> n > 1) counter in
  
  Corpus.fold_right 
    (fun sent_id graph (acc, acc_counter) ->
      let (new_sent_id, new_acc_counter) =
        match String_map.find_opt sent_id acc_counter with
        | None -> (sent_id, acc_counter)
        | Some i -> (sprintf "%s__%d" sent_id i, String_map.add sent_id (i-1) acc_counter) in
       (new_sent_id, `Assoc (List.map (fun (f,v) -> (f, `String v)) (Graph.get_meta_list graph))) :: acc,
       new_acc_counter
    ) corpus ([], amb_counter)
  |> (fun (x,_) -> `Assoc x)


let upload_corpus session_id file =
  let session = Session.get session_id in
  let config = session.conll_config in
  let log_file = Filename.concat (base_dir session_id) ((uid ())^".log") in
  let corpus = Corpus.from_file ~log_file ~config file in

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

  Session.update session_id
    (fun session ->
      { session with 
        corpus = Some corpus;
        graph=None;
        normal_forms=None;
        normal_form=None;
        history=None;
        position=None;
       }
    );
  `Assoc [
    ("meta_list", meta_list_from_corpus corpus);
    ("warnings", (`List warn_list))
  ]


let dep_save ?deco session_id graph =
  let session = Session.get session_id in
  let config = session.conll_config in
  let filter = Session.filter session in
  let dep = Graph.to_dep ~filter ?deco ~config graph in
  let d2p = Dep2pictlib.from_dep dep in
  let file = sprintf "%s.svg" (uid ()) in
  let filename = Filename.concat (images_dir session_id) file in
  Dep2pictlib.save_svg ~filename d2p;
  `String (Filename.concat (images_url session_id) file)

let dot_save ?deco session_id graph =
  let session = Session.get session_id in
  let config = session.conll_config in
  let dot = Graph.to_dot ?deco ~config graph in
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
  let session = Session.get session_id in
  match session.display with
  | Dep -> dep_save ?deco session_id graph
  | Dot -> dot_save ?deco session_id graph


let select_graph session_id sent_id =
  let session = Session.get session_id in
  match session.corpus with
  | None -> error "No corpus loaded"
  | Some corpus ->
    match Corpus.graph_of_sent_id sent_id corpus with
    | None -> error "No sent_id: %s" sent_id
    | Some graph -> 
      Session.update session_id
        (fun session ->
           { session with graph = Some graph; normal_forms=None; normal_form=None; history=None; position=None; }
        );
      graph_save session_id graph

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
  let session = Session.get session_id in
  let config = session.conll_config in
  let grs = Grs.load ~config file in
  Session.update session_id (fun session -> { session with grs = Some grs });
  `Assoc [exported_from_grs grs]

let rewrite session_id strat =
  let session = Session.get session_id in
  let config = session.conll_config in
  match (session.graph, session.grs) with
  | (None, _) -> error "No graph selected"
  | (_, None) -> error "No GRS loaded"
  | (Some graph, Some grs) ->
    Grewlib.set_track_history true;
    let graph_list = Rewrite.simple_rewrite ~config graph grs strat in
    let (log : Yojson.Basic.t) = Rewrite.log_rewrite () in
    Session.update session_id
      (fun session ->
         { session with normal_forms = Some graph_list; normal_form=None; history=None; position=None; }
      );
    `Assoc [
      ("normal_forms", `List (List.map (fun g -> `Int (Graph.trace_depth g)) graph_list));
      ("log", log)
    ]

let select_normal_form session_id position =
  let session = Session.get session_id in
  match session.normal_forms with
  | None -> error "Inconsistent_state [normal_forms]"
  | Some nfs ->
    match List.nth_opt nfs position with
    | None -> error "Inconsistent_state [position]"
    | Some graph ->
      Session.update session_id
        (fun session ->
           { session with normal_form = Some graph; history=None; position=None; }
        );
      graph_save session_id graph

let rules session_id =
  let session = Session.get session_id in
  match session.normal_form with
  | None -> error "No selected formal form"
  | Some graph ->
    let history = Graph.get_history graph in
    Session.update session_id
      (fun session ->
         { session with history = Some history; position=None; }
      );
    let rules = List.map (fun (_,(r,l),_,_) -> `List [`String r; `Int l]) history in
    `List rules

(* return the assoc list of things to (re)draw *)
let draw_before_after session_id =
  let session = Session.get session_id in
  match (session.history, session.position) with
  | (Some hist, Some pos) ->
    let ((graph_before, up_deco), (graph_after, down_deco))  =
      match CCList.drop pos hist with
      | [] -> error "No such item in history"
      | [(u,_,d,last)] ->
        (
          (last, u),
          ((match session.normal_form with Some g -> g | None -> error "Bug normal form"), d)
        )
      | (u,_,d,x)::(_,_,_,y)::_ -> ((x,u),(y,d)) in
    [
      ("before", graph_save ~deco:up_deco session_id graph_before);
      ("after", graph_save ~deco:down_deco session_id graph_after)
    ]
  | _ -> []

let select_rule session_id position =
  let session = Session.get session_id in
  match session.history with
  | None -> error "No history"
  | Some _ ->
    Session.update session_id (fun session -> { session with position = Some position });
    `Assoc (draw_before_after session_id)


let set_display session_id display =
  let session = Session.get session_id in
  Session.update session_id
    (fun session -> { session with display = if display = "graph" then Dot else Dep });
  `Assoc (
    draw_before_after session_id
    |> (fun l -> match session.graph with Some g -> ("init", graph_save session_id g) :: l | None -> l)
    |> (fun l -> match session.normal_form with Some g -> ("final", graph_save session_id g) :: l | None -> l)
  )

let upload_grs_code session_id code =
  let session = Session.get session_id in
  let config = session.conll_config in
  let grs = Grs.parse ~config code in
  Session.update session_id (fun session -> { session with grs = Some grs });
  `Assoc [exported_from_grs grs]



let url_corpus session_id url =
  let session = Session.get session_id in
  let config = session.conll_config in
  let ext = Filename.extension url in
  match Curly.(run (Request.make ~url ~meth:`GET ())) with
  | Error _ -> error "Fail to load grs on URL `%s`" url
  | Ok x ->
    match x.Curly.Response.code with
    | 200 ->
      let data = x.Curly.Response.body in
      let corpus = Corpus.from_string ~ext ~config data in 
      Session.update session_id
        (fun session ->
           { session with corpus = Some corpus;
                        graph=None; normal_forms=None; normal_form=None; history=None; position=None;
           }
        );
      (meta_list_from_corpus corpus)
    | 404 -> error "URL not found `%s`" url
    | code -> error "Network error %d on URL `%s`" code url

let url_grs session_id url =
  let session = Session.get session_id in
  let config = session.conll_config in
  match Curly.(run (Request.make ~url ~meth:`GET ())) with
  | Error _ -> error "Fail to load grs on URL `%s`" url
  | Ok x ->
    match x.Curly.Response.code with
    | 200 ->
      let data = x.Curly.Response.body in
      let grs = Grs.parse ~config data in
      Session.update session_id (fun session -> { session with grs = Some grs });
      `Assoc [
        ("code", `String data);
        exported_from_grs grs
      ]
    | 404 -> error "URL not found `%s`" url
    | code -> error "Network error %d on URL `%s`" code url

let get_grs session_id =
  let session = Session.get session_id in
  match session with
  | { grs = None; _ } -> `Null
  | { grs = Some g; _ } -> `Assoc [exported_from_grs g]

let get_corpus session_id =
  let session = Session.get session_id in
  match session.corpus with
  | None -> `Null
  | Some corpus ->
    `Assoc [
      ("meta_list", meta_list_from_corpus corpus);
      ("warnings", (`List []))
    ]

let save_normal_form session_id format =
  let session = Session.get session_id in
  let config = session.conll_config in
  match (session.normal_form, format) with
  | (Some nf, "json") ->
    let json = Graph.to_json nf in
    let file = sprintf "%s.json" (uid ()) in
    let filename = Filename.concat (images_dir session_id) file in
    let oc = open_out filename in 
    Yojson.Basic.pretty_to_channel oc json;
    close_out oc;
    `String (Filename.concat (images_url session_id) file)
  | (Some nf, "conll") ->
    let conll = nf |> Graph.to_json |> Conll.of_json |> Conll.to_string ~config in
    let file = sprintf "%s.conllu" (uid ()) in
    let filename = Filename.concat (images_dir session_id) file in
    CCIO.with_out filename (fun oc -> CCIO.write_line oc conll);
    `String (Filename.concat (images_url session_id) file)
  | (None, _) -> error "Inconsistent_state [normal_form]"
  | (Some _, _) -> error "Unknown format: %s" format
  

let split_path path =
  let rec loop p =
    match (Filename.dirname p, Filename.basename p) with
    | (".", _) -> ""
    | (p1, last) -> Filename.concat (loop p1) last in
  (loop (Filename.dirname path), Filename.basename path)


let upload_file session_id path tmp_file =
  let (subpath, file) = split_path path in
  let dir = Filename.concat (grs_dir session_id) subpath in
  FileUtil.mkdir ~parent:true dir;
  let _ = FileUtil.cp [tmp_file] (Filename.concat dir file) in
  `Null

let load_grs session_id grs_file =
  let session = Session.get session_id in
  let config = session.conll_config in
  FileUtil.mkdir ~parent:true (grs_dir session_id);
  let grs = Grs.load ~config (Filename.concat (grs_dir session_id) grs_file) in
  Session.update session_id (fun session -> { session with grs = Some grs });
  `Assoc [exported_from_grs grs]

