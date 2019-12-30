open Core
open Async
open Kraken
open Kraken_ws

let src = Logs.Src.create "kraken.ws-test" ~doc:"Kraken API - WS test application"
module Log_async = (val Logs_async.src_log src : Logs_async.LOG)

let process_user_cmd w =
  let process s =
    match String.split s ~on:' ' with
    | "unsubscribe" :: chanid :: _ ->
      let chanid = int_of_string chanid in
      Pipe.write w (Unsubscribe { chanid ; reqid = None })
    | "ping" :: v :: _ ->
      Pipe.write w (Ping (int_of_string_opt v))
    | "ping" :: _ ->
      Pipe.write w (Ping None)
    | "tickers" :: pair ->
      let pairs = List.map ~f:Pair.of_string_exn pair in
      Pipe.write w (tickers pairs)
    | "trades" :: pair ->
      let pairs = List.map ~f:Pair.of_string_exn pair in
      Pipe.write w (trades pairs)
    | "books" :: pair ->
      let pairs = List.map ~f:Pair.of_string_exn pair in
      Pipe.write w (book10 pairs)
    | h :: _ ->
      Log_async.err (fun m -> m "Unknown command %s" h)
    | [] ->
      Log_async.err (fun m -> m "Empty command")
  in
  let rec loop () = Reader.(read_line @@ Lazy.force stdin) >>= function
    | `Eof -> Deferred.unit
    | `Ok line -> process line >>= loop
  in
  loop ()

module Cfg = struct
  type cfg = {
    key: string ;
    secret: string ;
    passphrase: string [@default ""];
    quote: (string * int) list [@default []];
  } [@@deriving sexp]

  type t = (string * cfg) list [@@deriving sexp]
end

let default_cfg = Filename.concat (Option.value_exn (Sys.getenv "HOME")) ".virtu"
let cfg =
  List.Assoc.find_exn ~equal:String.equal
    (Sexplib.Sexp.load_sexp_conv_exn default_cfg Cfg.t_of_sexp) "KRAKEN"

let auth = Fastrest.auth ~key:cfg.key ~secret:(Base64.decode_exn cfg.secret) ()

let main () =
  Fastrest.request ~auth Kraken_rest.websocket_token >>|? fun { token; _ } ->
  Fastws_async.with_connection ~of_string ~to_string url_public begin fun _ r w ->
    let log_incoming msg =
      Logs_async.debug ~src (fun m -> m "%a" pp msg) in
    Pipe.write w (ownTrades token) >>= fun () ->
    Pipe.write w (openOrders token) >>= fun () ->
    Deferred.all_unit [
      process_user_cmd w ;
      Pipe.iter r ~f:log_incoming
    ]
  end

let () =
  Command.async ~summary:"Kraken WS client" begin
    let open Command.Let_syntax in
    [%map_open
      let () = Logs_async_reporter.set_level_via_param [] in
      fun () ->
        Logs.set_reporter (Logs_async_reporter.reporter ()) ;
        main () >>= function
        | Error e -> Error.raise e
        | Ok _ -> Deferred.unit
    ] end |>
  Command.run
