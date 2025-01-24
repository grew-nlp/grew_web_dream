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

let all_routes = 
  [
    ping_route;
    connect_route;
    upload_corpus_route;
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
 
module Grew_web_back_app =
  Eliom_registration.App (
  struct
    let application_name = "grew_web_back"
    let global_data_path = None
  end)

let main_service =
  Eliom_service.create
    ~path:(Eliom_service.Path [])
    ~meth:(Eliom_service.Get Eliom_parameter.unit)
    ()

  let () =
    Grew_web_back_app.register
      ~service:main_service
      (fun () () ->
         Lwt.return
           (Eliom_tools.F.html
              ~title:"grew_web_back"
              ~css:[["css";"grew_web_back.css"]]
              Html.F.(body [
                  h1 [txt "Welcome from Eliom's distillery!"];
                ])))




let _ = Eliom_registration.String.create
    ~path:(Eliom_service.Path ["set_display"])
    ~meth:(Eliom_service.Post (
        Eliom_parameter.unit,
        Eliom_parameter.(string "session_id" ** string "display")
      ))
    (fun () (session_id, display) ->
       Log.info "[session_id=%s] <set_display> display=%s" session_id display;
       let json = wrap (set_display session_id) display in
       Lwt.return (Yojson.Basic.pretty_to_string json, "text/plain")
    )

let _ = Eliom_registration.String.create
    ~path:(Eliom_service.Path ["upload_corpus"])
    ~meth:(Eliom_service.Post (
        Eliom_parameter.unit,
        Eliom_parameter.(string "session_id" ** file "file")
      ))
    (fun () (session_id, file) ->
       Log.info "[session_id=%s] <upload_corpus>" session_id;
       let json = wrap (upload_corpus session_id) file in
       Lwt.return (Yojson.Basic.pretty_to_string json, "text/plain")
    )

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
    ~path:(Eliom_service.Path ["upload_grs"])
    ~meth:(Eliom_service.Post (
        Eliom_parameter.unit,
        Eliom_parameter.(string "session_id" ** file "file")
      ))
    (fun () (session_id, file) ->
       Log.info "[session_id=%s] <upload_grs>" session_id;
       let json = wrap (upload_grs session_id) file in
       Lwt.return (Yojson.Basic.pretty_to_string json, "text/plain")
    )

let _ = Eliom_registration.String.create
    ~path:(Eliom_service.Path ["upload_grs"])
    ~meth:(Eliom_service.Post (
        Eliom_parameter.unit,
        Eliom_parameter.(string "session_id" ** file "json_file")
      ))
    (fun () (session_id, json_file) ->
       Log.info "[session_id=%s] <upload_grs>" session_id;
       let json = wrap (upload_json_grs session_id) json_file in
       Lwt.return (Yojson.Basic.pretty_to_string json, "text/plain")
    )

let _ = Eliom_registration.String.create
    ~path:(Eliom_service.Path ["upload_grs_code"])
    ~meth:(Eliom_service.Post (
        Eliom_parameter.unit,
        Eliom_parameter.(string "session_id" ** string "code")
      ))
    (fun () (session_id, code) ->
       Log.info "[session_id=%s] <upload_grs_code>" session_id;
       let json = wrap (upload_grs_code session_id) code in
       Lwt.return (Yojson.Basic.pretty_to_string json, "text/plain")
    )

let _ = Eliom_registration.String.create
    ~path:(Eliom_service.Path ["url_grs"])
    ~meth:(Eliom_service.Post (
        Eliom_parameter.unit,
        Eliom_parameter.(string "session_id" ** string "url")
      ))
    (fun () (session_id, url) ->
       Log.info "[session_id=%s] <url_grs> url=%s" session_id url;
       let json = wrap (url_grs session_id) url in
       Lwt.return (Yojson.Basic.pretty_to_string json, "text/plain")
    )

let _ = Eliom_registration.String.create
    ~path:(Eliom_service.Path ["select_graph"])
    ~meth:(Eliom_service.Post (
        Eliom_parameter.unit,
        Eliom_parameter.(string "session_id" ** string "sent_id")
      ))
    (fun () (session_id, sent_id) ->
       Log.info "[session_id=%s] <select_graph> sent_id=%s" session_id sent_id;
       let json = wrap (select_graph session_id) sent_id in
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

    let _ = Eliom_registration.String.create
    ~path:(Eliom_service.Path ["from_data"])
    ~meth:(Eliom_service.Post (
        Eliom_parameter.unit,
        Eliom_parameter.(string "conll")
      ))
    (fun () conll ->
      Log.info "<from_data> conll=%s" conll;
      let json = wrap (from_data ~conll) () in
      Lwt.return (Yojson.Basic.pretty_to_string json, "text/plain")
    )

    let _ = Eliom_registration.String.create
    ~path:(Eliom_service.Path ["from_data"])
    ~meth:(Eliom_service.Post (
        Eliom_parameter.unit,
        Eliom_parameter.(string "json")
      ))
    (fun () json ->
      Log.info "<from_data> json=%s" json;
      let json = wrap (from_data ~json) () in
      Lwt.return (Yojson.Basic.pretty_to_string json, "text/plain")
    )

    let _ = Eliom_registration.String.create
    ~path:(Eliom_service.Path ["from_data"])
    ~meth:(Eliom_service.Post (
        Eliom_parameter.unit,
        Eliom_parameter.(string "conll" ** string "grs")
      ))
    (fun () (conll, grs) ->
      Log.info "<from_data> conll=%s grs=%s" conll grs;
      let json = wrap (from_data ~conll ~grs) () in
      Lwt.return (Yojson.Basic.pretty_to_string json, "text/plain")
    )

    let _ = Eliom_registration.String.create
    ~path:(Eliom_service.Path ["from_data"])
    ~meth:(Eliom_service.Post (
        Eliom_parameter.unit,
        Eliom_parameter.(string "json" ** string "grs")
      ))
    (fun () (json, grs) ->
      Log.info "<from_data> json=%s grs=%s" json grs;
      let json = wrap (from_data ~json ~grs) () in
      Lwt.return (Yojson.Basic.pretty_to_string json, "text/plain")
    )
 *)
