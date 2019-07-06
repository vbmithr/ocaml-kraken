open Core
open Async

open Kraken
open Kraken_ws

let url = Uri.make ~scheme:"https" ~host:"ws-beta.kraken.com" ()
let sandbox_url = Uri.make ~scheme:"https" ~host:"ws-sandbox.kraken.com" ()

let src = Logs.Src.create "kraken.ws.async"
module Log = (val Logs.src_log src : Logs.LOG)

let connect ?(sandbox=false) () =
  let url = if sandbox then sandbox_url else url in
  Fastws_async.connect_ez url >>|
  Result.map ~f:begin fun (r, w, cleaned_up) ->
    let client_read = Pipe.map r ~f:begin fun msg ->
        Ezjsonm_encoding.destruct_safe encoding (Ezjsonm.from_string msg)
      end in
    let ws_read, client_write = Pipe.create () in
    don't_wait_for
      (Pipe.closed client_write >>| fun () -> Pipe.close w) ;
    don't_wait_for @@
    Pipe.transfer ws_read w ~f:begin fun cmd ->
      let doc =
        match Ezjsonm_encoding.construct encoding cmd with
        | `A _ | `O _ as a -> Ezjsonm.to_string a
        | _ -> invalid_arg "not a json document" in
      Log.debug (fun m -> m "-> %s" doc) ;
      doc
    end ;
    (client_read, client_write, cleaned_up)
  end

let connect_exn ?sandbox () =
  connect ?sandbox () >>= function
  | Error `Internal exn -> raise exn
  | Error `WS e -> Fastws_async.raise_error e
  | Ok a -> return a

let with_connection ?(sandbox=false) f =
  let url = if sandbox then sandbox_url else url in
  Fastws_async.with_connection_ez url ~f:begin fun r w ->
    let r = Pipe.map r ~f:begin fun msg ->
        Ezjsonm_encoding.destruct_safe encoding (Ezjsonm.from_string msg)
      end in
    let ws_read, client_write = Pipe.create () in
    don't_wait_for @@
    Pipe.transfer ws_read w ~f:begin fun cmd ->
      let doc =
        match Ezjsonm_encoding.construct encoding cmd with
        | `A _ | `O _ as a -> Ezjsonm.to_string a
        | _ -> invalid_arg "not a json document" in
      Log.debug (fun m -> m "-> %s" doc) ;
      doc
    end ;
    Monitor.protect
      (fun () -> f r client_write)
      ~finally:(fun () -> Pipe.close_read ws_read ; Deferred.unit)
  end

let with_connection_exn ?sandbox f =
  with_connection ?sandbox f >>= function
  | Error `Internal exn -> raise exn
  | Error `User_callback exn -> raise exn
  | Error `WS e -> Fastws_async.raise_error e
  | Ok a -> return a
