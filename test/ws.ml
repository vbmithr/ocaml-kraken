open Core
open Async

open Kraken_ws
open Kraken_ws_async

let src = Logs.Src.create "kraken.ws-test"
    ~doc:"Kraken API - WS test application"

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
    | "trades" :: pair ->
      Pipe.write w (Subscribe { reqid = None ; pair ; sub = Trade })
    | "books" :: pair ->
      Pipe.write w (Subscribe { reqid = None ; pair ; sub = Book 10 })
    | h :: _ ->
      Logs_async.err (fun m -> m "Unknown command %s" h)
    | [] ->
      Logs_async.err ~src (fun m -> m "Empty command")
  in
  let rec loop () = Reader.(read_line @@ Lazy.force stdin) >>= function
    | `Eof -> Deferred.unit
    | `Ok line -> process line >>= loop
  in
  loop ()

let main () =
  with_connection begin fun r w ->
    let log_incoming msg =
      Logs_async.debug ~src (fun m -> m "%a" pp msg) in
    Deferred.all_unit [
      process_user_cmd w ;
      Pipe.iter r ~f:log_incoming
    ]
  end

let () =
  Command.async ~summary:"Kraken WS client" begin
    let open Command.Let_syntax in
    [%map_open
      let () = Logs_async_reporter.set_level_via_param None in
      fun () ->
        Logs.set_reporter (Logs_async_reporter.reporter ()) ;
        main ()
    ] end |>
  Command.run
