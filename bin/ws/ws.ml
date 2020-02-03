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
    | "ping" :: _ ->
      Pipe.write w (ping (Time_ns.(now () |> to_span_since_epoch |> Span.to_us)))
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

let key, secret =
  match String.split ~on:':' (Sys.getenv_exn "TOKEN_KRAKEN") with
  | [key; secret] -> key, secret
  | _ -> assert false

let auth =
  Fastrest.auth ~key ~secret:(Base64.decode_exn secret) ()

let main () =
  Fastrest.request ~auth Kraken_rest.websocket_token >>= fun { token; _ } ->
  Fastws_async.with_connection ~of_string ~to_string url_auth begin fun r w ->
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
        main ()
    ] end |>
  Command.run
