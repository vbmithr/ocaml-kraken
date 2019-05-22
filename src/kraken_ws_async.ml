open Core
open Async

open Kraken
open Kraken_ws

let url = Uri.make ~scheme:"https" ~host:"ws.kraken.com" ()
let sandbox_url = Uri.make ~scheme:"https" ~host:"ws-sandbox.kraken.com" ()

let src = Logs.Src.create "kraken.ws.async"
module Log = (val Logs.src_log src : Logs.LOG)

let connect ?(sandbox=false) () =
  let url = if sandbox then sandbox_url else url in
  Fastws_async.connect_ez url >>= fun (r, w, cleaned_up) ->
  let c = Lazy.force Time_stamp_counter.calibrator in
  let stop = Deferred.any_unit [Pipe.closed r ; Pipe.closed w] in
  (* Calibration *)
  Clock_ns.every ~stop (Time_ns.Span.of_int_sec 60) begin fun () ->
    Time_stamp_counter.Calibrator.calibrate c
  end ;
  let last_ts = ref (Time_stamp_counter.now ()) in
  let client_read = Pipe.map' r ~f:begin fun msgq ->
      last_ts := Time_stamp_counter.now () ;
      return @@ Queue.map msgq ~f:begin fun msg ->
        !last_ts,
        Ezjsonm_encoding.destruct_safe encoding (Ezjsonm.from_string msg)
      end
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
  return (client_read, client_write, cleaned_up)

let with_connection ?(sandbox=false) f =
  let url = if sandbox then sandbox_url else url in
  Fastws_async.with_connection_ez url ~f:begin fun r w ->
    let c = Lazy.force Time_stamp_counter.calibrator in
    let stop = Deferred.any_unit [Pipe.closed r ; Pipe.closed w] in
    (* Calibration *)
    Clock_ns.every ~stop (Time_ns.Span.of_int_sec 60) begin fun () ->
      Time_stamp_counter.Calibrator.calibrate c
    end ;
    let last_ts = ref (Time_stamp_counter.now ()) in
    let r = Pipe.map' r ~f:begin fun msgq ->
        last_ts := Time_stamp_counter.now () ;
        return @@ Queue.map msgq ~f:begin fun msg ->
          !last_ts,
          Ezjsonm_encoding.destruct_safe encoding (Ezjsonm.from_string msg)
        end
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
