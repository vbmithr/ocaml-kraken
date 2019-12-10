open Core
open Async

let src = Logs.Src.create "kraken.compta"
module Log_async = (val Logs_async.src_log src : Logs_async.LOG)

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

open Kraken


let url = Uri.make ~scheme:"kdb" ~host:"localhost" ~port:5042 ()

let side_of_string = function "buy" -> Fixtypes.Side.Buy | _ -> Sell
let string_of_side = function Fixtypes.Side.Buy -> "buy" | Sell -> "sell"

let ordType_of_string = function "limit" -> Fixtypes.OrdType.Limit | _ -> Market
let string_of_ordType = function Fixtypes.OrdType.Limit -> "limit" | _ -> "market"

let sides_encoding =
  let open Kx in
  conv
    (Array.map ~f:string_of_side)
    (Array.map ~f:side_of_string)
    (v sym)

let ordTypes_encoding =
  let open Kx in
  conv
    (Array.map ~f:string_of_ordType)
    (Array.map ~f:ordType_of_string)
    (v sym)

let krakids =
  Kx.(conv  (Array.map ~f:KrakID.to_guid) (Array.map ~f:KrakID.of_guid) (v guid))

let line =
  let open Kx in
  t7 (v timestamp) (v sym) krakids sides_encoding ordTypes_encoding (v float) (v float)

let kx_of_fills fills =
  let len = List.length fills in
  let times = Array.create ~len Ptime.epoch in
  let syms = Array.create ~len "" in
  let tids = Array.create ~len KrakID.zero in
  let sides = Array.create ~len Fixtypes.Side.Buy in
  let ordTypes = Array.create ~len Fixtypes.OrdType.Market in
  let pxs = Array.create ~len Float.nan in
  let qties = Array.create ~len Float.nan in
  List.iteri fills ~f:begin fun i ({ ordertxid; pair; time; side; ord_type; price; vol; _ }:Filled_order.t) ->
    times.(i) <- time ;
    syms.(i) <- pair ;
    tids.(i) <- ordertxid ;
    sides.(i) <- side ;
    ordTypes.(i) <- ord_type ;
    pxs.(i) <- price ;
    qties.(i) <- vol ;
  end ;
  let open Kx in
  Kx_async.create (t3 (a sym) (a sym) line) ("upd", "trades", (times, syms, tids, sides, ordTypes, pxs, qties))

let auth = {
  Fastrest.key = cfg.key ;
  secret = Base64.decode_exn cfg.secret ;
  meta = []
}

let retrieveFills w =
  let rec inner n =
    Fastrest.request ~auth (Kraken_rest.trade_history n) >>=? fun fills ->
    Pipe.write w (kx_of_fills fills) >>= fun () ->
    let len = List.length fills in
    Logs_async.app (fun m -> m "Found %d fills" len) >>= fun () ->
    Deferred.List.iter fills ~f:begin fun fill ->
      Log_async.app (fun m -> m "%a" Filled_order.pp fill)
    end >>= fun () ->
    if len < 50 then Deferred.Or_error.ok_unit
    else inner (n + len) in
  inner 0

let retrieveLedgers _w =
  let rec inner n =
    Fastrest.request ~auth (Kraken_rest.ledgers ~ofs:n ()) >>=? fun ledgers ->
    (* Pipe.write w (kx_of_fills fills) >>= fun () -> *)
    let len = List.length ledgers in
    Logs_async.app (fun m -> m "Found %d ledger entries" len) >>= fun () ->
    Deferred.List.iter ledgers ~f:begin fun l ->
      Log_async.app (fun m -> m "%a" Ledger.pp l)
    end >>= fun () ->
    if len < 50 then Deferred.Or_error.ok_unit
    else inner (n + len) in
  inner 0

let main () =
  Kx_async.Async.with_connection url ~f:begin fun { w; _ } ->
    retrieveFills w >>=? fun () ->
    retrieveLedgers w
  end >>= fun _ ->
  Deferred.unit

let () =
  Command.async ~summary:"Kraken kdb+ compta" begin
    let open Command.Let_syntax in
    [%map_open
      let () = Logs_async_reporter.set_level_via_param [] in
      (* and ledger = flag "ledger" no_arg ~doc:"Record ledger entries in DB" in *)
      fun () ->
        Logs.set_reporter (Logs_async_reporter.reporter ()) ;
        main ()
    ] end |>
  Command.run
