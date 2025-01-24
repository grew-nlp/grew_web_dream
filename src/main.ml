open Grewlib
open Dream_utils
open Gwd_services


let _build_option service_name =
  Dream.options service_name (fun _req ->
    Dream.respond ~headers:[ ("Allow", "OPTIONS, GET, HEAD, POST") ] ""
  )

let cors_middleware handler req =
      let handlers =
        [ "Allow", "OPTIONS, GET, HEAD, POST"
        ; "Access-Control-Allow-Origin", "*"
        ; "Access-Control-Allow-Methods", "OPTIONS, GET, HEAD, POST"
        ; "Access-Control-Allow-Headers", "Content-Type"
        ; "Access-Control-Max-Age", "86400"
        ]
      in
      let%lwt res = handler req in
      handlers
      |> List.map (fun (key, value) -> Dream.add_header res key value)
      |> ignore;
      Lwt.return res

let ping_route =
  Dream.post "ping" (fun _ -> Dream.html ~headers:["Content-Type", "text/plain"] "{}")

let static_route =
  Dream.get "/**" (Dream.static "static")

let connect_route =
  Dream.post "connect"
    (fun request ->
      match%lwt Dream.body request with
      | "{}" ->
        let json = wrap connect () in
        Log.info "<connect> ==> %s" (report_status json);
        reply json
      | _ -> Dream.empty `Bad_Request
    )

let upload_corpus_route = 
  Dream.post "upload_corpus"
    (fun request ->
      match%lwt stream_request request with
      | (map,[(filename,tmp_file)]) ->
        let session_id = String_map.find "session_id" map in
        let file = Filename.concat (base_dir session_id) filename in
        FileUtil.mv tmp_file file;
        let json = wrap (upload_corpus session_id) file in
        (* Log.info "<upload_corpus> project_id=[%s] sample_id=[%s] ==> %s" project_id sample_id (report_status json); *)
        reply json
      | (_,l) ->
        reply_error "<upload_corpus> received %d files (1 expected)" (List.length l)
    )

let select_graph_route =
  let open Yojson.Basic.Util in
  Dream.post "select_graph"
    (fun request ->
      let%lwt body = Dream.body request in
      let param = body |> Yojson.Basic.from_string |> to_assoc in
      let session_id = List.assoc "session_id" param |> to_string in
      let sent_id = List.assoc "sent_id" param |> to_string in
      let json = wrap (select_graph session_id) sent_id in
      Log.info "<select_graph> ==> %s" (report_status json);
        reply json
    )

let upload_grs_route = 
  Dream.post "upload_grs"
    (fun request ->
      match%lwt stream_request request with
      | (map,[(filename,tmp_file)]) ->
        let session_id = String_map.find "session_id" map in
        let file = Filename.concat (base_dir session_id) filename in
        FileUtil.mv tmp_file file;
        let json = wrap (upload_grs session_id) file in
        (* Log.info "<upload_grs> project_id=[%s] sample_id=[%s] ==> %s" project_id sample_id (report_status json); *)
        reply json
      | (_,l) ->
        reply_error "<upload_grs> received %d files (1 expected)" (List.length l)
    )

let rewrite_route =
  let open Yojson.Basic.Util in
  Dream.post "rewrite"
    (fun request ->
      let%lwt body = Dream.body request in
      Printf.printf "===%s===\n%!" body;
      let param = body |> Yojson.Basic.from_string |> to_assoc in
      let session_id = List.assoc "session_id" param |> to_string in
      let strat = List.assoc "strat" param |> to_string in
      let json = wrap (rewrite session_id) strat in
      Log.info "<rewrite> ==> %s" (report_status json);
        reply json
    )

let select_normal_form_route =
  let open Yojson.Basic.Util in
  Dream.post "select_normal_form"
    (fun request ->
      let%lwt body = Dream.body request in
      Printf.printf "===%s===\n%!" body;
      let param = body |> Yojson.Basic.from_string |> to_assoc in
      let session_id = List.assoc "session_id" param |> to_string in
      let position = List.assoc "position" param |> to_string in
      let json = wrap (select_normal_form session_id) position in
      Log.info "<select_normal_form> ==> %s" (report_status json);
        reply json
    )

let rules_route =
  let open Yojson.Basic.Util in
  Dream.post "rules"
    (fun request ->
      let%lwt body = Dream.body request in
      Printf.printf "===%s===\n%!" body;
      let param = body |> Yojson.Basic.from_string |> to_assoc in
      let session_id = List.assoc "session_id" param |> to_string in
      let json = wrap rules session_id in
      Log.info "<rules> ==> %s" (report_status json);
        reply json
    )

let select_rule_route =
  let open Yojson.Basic.Util in
  Dream.post "select_rule"
    (fun request ->
      let%lwt body = Dream.body request in
      Printf.printf "===%s===\n%!" body;
      let param = body |> Yojson.Basic.from_string |> to_assoc in
      let session_id = List.assoc "session_id" param |> to_string in
      let position = List.assoc "position" param |> to_int in
      let json = wrap (select_rule session_id) position in
      Log.info "<select_rule> ==> %s" (report_status json);
        reply json
    )


let set_display_route =
  let open Yojson.Basic.Util in
  Dream.post "set_display"
    (fun request ->
      let%lwt body = Dream.body request in
      Printf.printf "===%s===\n%!" body;
      let param = body |> Yojson.Basic.from_string |> to_assoc in
      let session_id = List.assoc "session_id" param |> to_string in
      let display = List.assoc "display" param |> to_string in
      let json = wrap (set_display session_id) display in
      Log.info "<set_display> ==> %s" (report_status json);
        reply json
    )

let upload_grs_code_route =
  let open Yojson.Basic.Util in
  Dream.post "upload_grs_code"
    (fun request ->
      let%lwt body = Dream.body request in
      Printf.printf "===%s===\n%!" body;
      let param = body |> Yojson.Basic.from_string |> to_assoc in
      let session_id = List.assoc "session_id" param |> to_string in
      let code = List.assoc "code" param |> to_string in
      let json = wrap (upload_grs_code session_id) code in
      Log.info "<upload_grs_code> ==> %s" (report_status json);
        reply json
    )

let url_grs_route =
  let open Yojson.Basic.Util in
  Dream.post "url_grs"
    (fun request ->
      let%lwt body = Dream.body request in
      Printf.printf "===%s===\n%!" body;
      let param = body |> Yojson.Basic.from_string |> to_assoc in
      let session_id = List.assoc "session_id" param |> to_string in
      let url = List.assoc "url" param |> to_string in
      let json = wrap (url_grs session_id) url in
      Log.info "<url_grs> ==> %s" (report_status json);
        reply json
    )

let url_corpus_route =
  let open Yojson.Basic.Util in
  Dream.post "url_corpus"
    (fun request ->
      let%lwt body = Dream.body request in
      Printf.printf "===%s===\n%!" body;
      let param = body |> Yojson.Basic.from_string |> to_assoc in
      let session_id = List.assoc "session_id" param |> to_string in
      let url = List.assoc "url" param |> to_string in
      let json = wrap (url_corpus session_id) url in
      Log.info "<url_corpus> ==> %s" (report_status json);
        reply json
    )

let get_grs_route =
  let open Yojson.Basic.Util in
  Dream.post "get_grs"
    (fun request ->
      let%lwt body = Dream.body request in
      Printf.printf "===%s===\n%!" body;
      let param = body |> Yojson.Basic.from_string |> to_assoc in
      let session_id = List.assoc "session_id" param |> to_string in
      let json = wrap get_grs session_id in
      Log.info "<get_grs> ==> %s" (report_status json);
        reply json
    )

let get_corpus_route =
  let open Yojson.Basic.Util in
  Dream.post "get_corpus"
    (fun request ->
      let%lwt body = Dream.body request in
      Printf.printf "===%s===\n%!" body;
      let param = body |> Yojson.Basic.from_string |> to_assoc in
      let session_id = List.assoc "session_id" param |> to_string in
      let json = wrap get_corpus session_id in
      Log.info "<get_corpus> ==> %s" (report_status json);
        reply json
    )

let save_normal_form_route =
  let open Yojson.Basic.Util in
  Dream.post "save_normal_form"
    (fun request ->
      let%lwt body = Dream.body request in
      Printf.printf "===%s===\n%!" body;
      let param = body |> Yojson.Basic.from_string |> to_assoc in
      let session_id = List.assoc "session_id" param |> to_string in
      let format = List.assoc "format" param |> to_string in
      let json = wrap (save_normal_form session_id) format in
      Log.info "<save_normal_form> ==> %s" (report_status json);
        reply json
    )

let all_routes = 
  [
    ping_route;
    static_route;
    connect_route;
    upload_corpus_route;
    select_graph_route;
    upload_grs_route;
    upload_grs_code_route;
    rewrite_route;
    select_normal_form_route;
    rules_route;
    select_rule_route;
    set_display_route;
    url_grs_route;
    url_corpus_route;
    get_grs_route;
    get_corpus_route;
    save_normal_form_route;
  ]


let _ =
  try
    Random.self_init ();
    let required = ["port"] in
    Dream_config.load ~required ();
    Log.init ?prefix:(Dream_config.get_string_opt "prefix") ();
    (* let _ = load_data () in
    let _ = refresh () in *)

    Printf.printf "|current|=%d\n%!" (List.length (Dream_config.get_env ()));
    List.iter (fun (x,y) -> Printf.printf "%s --> %s\n%!" x y) (Dream_config.get_env ());

    Dream.run
    ~error_handler:Dream.debug_error_handler
    ~port: (Dream_config.get_int "port")
    @@ cors_middleware
    @@ Dream.logger
    @@ Dream.router all_routes
  with Error msg -> 
    (* stop "%s" (Yojson.Basic.pretty_to_string msg) *)
    stop "%s" (Yojson.Basic.pretty_to_string msg)


(* 
 

let _ = Eliom_registration.String.create
    ~path:(Eliom_service.Path ["upload_file"])
    ~meth:(Eliom_service.Post (
        Eliom_parameter.unit,
        Eliom_parameter.(string "session_id" ** (string "path" ** file "file"))
      ))
    (fun () (session_id, (path, file)) ->
       Log.info "[session_id=%s] <upload_file> path=%s" session_id path;
       let json = wrap (upload_file session_id path) file in
       Lwt.return (Yojson.Basic.pretty_to_string json, "text/plain")
    )

let _ = Eliom_registration.String.create
    ~path:(Eliom_service.Path ["load_grs"])
    ~meth:(Eliom_service.Post (
        Eliom_parameter.unit,
        Eliom_parameter.(string "session_id" ** string "grs_file")
      ))
    (fun () (session_id, grs_file) ->
       Log.info "[session_id=%s] <load_grs>" session_id;
       let json = wrap (load_grs session_id) grs_file in
       Lwt.return (Yojson.Basic.pretty_to_string json, "text/plain")
    )

let _ = Eliom_registration.String.create
    ~path:(Eliom_service.Path ["url_corpus"])
    ~meth:(Eliom_service.Post (
        Eliom_parameter.unit,
        Eliom_parameter.(string "session_id" ** string "url")
      ))
    (fun () (session_id, url) ->
       Log.info "[session_id=%s] <url_corpus> url=%s" session_id url;
       let json = wrap (url_corpus session_id) url in
       Lwt.return (Yojson.Basic.pretty_to_string json, "text/plain")
    )


let _ = Eliom_registration.String.create
    ~path:(Eliom_service.Path ["rewrite"])
    ~meth:(Eliom_service.Post (
        Eliom_parameter.unit,
        Eliom_parameter.(string "session_id" ** string "strat")
      ))
    (fun () (session_id, strat) ->
       Log.info "[session_id=%s] <rewrite> strat=%s" session_id strat;
       let json = wrap (rewrite session_id) strat in
       Lwt.return (Yojson.Basic.pretty_to_string json, "text/plain")
    )

let _ = Eliom_registration.String.create
    ~path:(Eliom_service.Path ["select_normal_form"])
    ~meth:(Eliom_service.Post (
        Eliom_parameter.unit,
        Eliom_parameter.(string "session_id" ** string "position")
      ))
    (fun () (session_id, position) ->
       Log.info "[session_id=%s] <select_normal_form> position=%s" session_id position;
       let json = wrap (select_normal_form session_id) position in
       Lwt.return (Yojson.Basic.pretty_to_string json, "text/plain")
    )

let _ = Eliom_registration.String.create
    ~path:(Eliom_service.Path ["save_normal_form"])
    ~meth:(Eliom_service.Post (
        Eliom_parameter.unit,
        Eliom_parameter.(string "session_id" ** string "format")
      ))
    (fun () (session_id, format) ->
       Log.info "[session_id=%s] <save_normal_form> format=%s" session_id format;
       let json = wrap (save_normal_form session_id) format in
       Lwt.return (Yojson.Basic.pretty_to_string json, "text/plain")
    )

let _ = Eliom_registration.String.create
    ~path:(Eliom_service.Path ["rules"])
    ~meth:(Eliom_service.Post (
        Eliom_parameter.unit,
        Eliom_parameter.(string "session_id")
      ))
    (fun () session_id ->
       Log.info "[session_id=%s] <rules>" session_id;
       let json = wrap rules session_id in
       Lwt.return (Yojson.Basic.pretty_to_string json, "text/plain")
    )

let _ = Eliom_registration.String.create
    ~path:(Eliom_service.Path ["select_rule"])
    ~meth:(Eliom_service.Post (
        Eliom_parameter.unit,
        Eliom_parameter.(string "session_id" ** string "position")
      ))
    (fun () (session_id, position) ->
       Log.info "[session_id=%s] <select_rule> position=%s" session_id position;
       let json = wrap (select_rule session_id) position in
       Lwt.return (Yojson.Basic.pretty_to_string json, "text/plain")
    )

let _ = Eliom_registration.String.create
    ~path:(Eliom_service.Path ["get_corpus"])
    ~meth:(Eliom_service.Post (
        Eliom_parameter.unit,
        Eliom_parameter.(string "session_id")
      ))
    (fun () session_id ->
       Log.info "[session_id=%s] <get_corpus>" session_id;
       let json = wrap get_corpus session_id in
       Lwt.return (Yojson.Basic.pretty_to_string json, "text/plain")
    )

    let _ = Eliom_registration.String.create
    ~path:(Eliom_service.Path ["get_grs"])
    ~meth:(Eliom_service.Post (
        Eliom_parameter.unit,
        Eliom_parameter.(string "session_id")
      ))
    (fun () session_id ->
       Log.info "[session_id=%s] <get_grs>" session_id;
       let json = wrap get_grs session_id in
       Lwt.return (Yojson.Basic.pretty_to_string json, "text/plain")
    )

 *)
