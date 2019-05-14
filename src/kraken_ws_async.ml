open Core
open Async

open Kraken
open Kraken_ws

let url = Uri.make ~scheme:"https" ~host:"ws.kraken.com" ()
let sandbox_url = Uri.make ~scheme:"https" ~host:"ws-sandbox.kraken.com" ()

exception Timeout

let with_connection ?(sandbox=false) f =
  let url = if sandbox then sandbox_url else url in
  Fastws_async.with_connection_ez url ~f:begin fun r w ->
    let c = Lazy.force Time_stamp_counter.calibrator in
    let stop = Deferred.any_unit [Pipe.closed r ; Pipe.closed w] in
    (* Calibration *)
    Clock_ns.every ~stop
      (Time_ns.Span.of_int_sec 60)
      (fun () -> Time_stamp_counter.Calibrator.calibrate c) ;
    let last_ts = ref (Time_stamp_counter.now ()) in
    (* Every 10 seconds, check that we got a message in the latest 10
       seconds. *)
    Clock_ns.every ~stop
      (Time_ns.Span.of_int_sec 10)
      begin fun () ->
        if Time_stamp_counter.(Span.to_ns ~calibrator:c (diff (now ()) !last_ts)) >
           Int63.(of_int 10_000_000_000) then raise Timeout
      end ;
    let r = Pipe.map r ~f:begin fun msg ->
        last_ts := Time_stamp_counter.now () ;
        !last_ts,
        Ezjsonm_encoding.destruct_safe encoding (Ezjsonm.from_string msg)
      end in
    let ws_read, client_write = Pipe.create () in
    don't_wait_for @@
    Pipe.transfer ws_read w ~f:begin fun cmd ->
      let doc =
        match Ezjsonm_encoding.construct encoding cmd with
        | `A _ | `O _ as a -> Ezjsonm.to_string a
        | _ -> invalid_arg "not a json document" in
      Logs.debug (fun m -> m "-> %s" doc) ;
      doc
    end ;
    Monitor.protect
      (fun () -> f r client_write)
      ~finally:(fun () -> Pipe.close_read ws_read ; Deferred.unit)
  end
