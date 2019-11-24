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

let line =
  let open Kx in
  t7 (v timestamp) (v sym) (list (s char)) sides_encoding ordTypes_encoding
    (v float) (v float)

let kx_of_fills fills =
  let (times,syms,tids,sides,ordTypes,prices,qties) =
    List.fold_right fills ~init:([],[],[],[],[],[],[])
      ~f:begin fun (tid, fill) (times,syms,tids,sides,ordTypes,prices,qties) ->
        (fill.Filled_order.time :: times,
         fill.pair :: syms,
         tid :: tids,
         fill.side :: sides,
         fill.ord_type :: ordTypes,
         fill.price :: prices,
         fill.vol :: qties)
      end in
  Kx_async.create line Array.(of_list times, of_list syms, tids, of_list sides,
                              of_list ordTypes, of_list prices, of_list qties)

let main () =
  Kx_async.Async.with_connection url ~f:begin fun { w; _ } ->
    let rec inner n =
      Fastrest.request
        ~auth:{ Fastrest.key = cfg.key ;
                secret = Base64.decode_exn cfg.secret ;
                meta = [] } (Kraken_rest.trade_history n) >>=? fun fills ->
      Pipe.write w (kx_of_fills fills) >>= fun () ->
      let len = List.length fills in
      Logs_async.app (fun m -> m "Found %d fills" len) >>= fun () ->
      Deferred.List.iter fills ~f:begin fun (str, fill) ->
        Log_async.app (fun m -> m "%s %a" str Filled_order.pp fill)
      end >>= fun () ->
      if len < 50 then Deferred.Or_error.ok_unit
      else inner (n + len) in
    inner 0
  end >>= fun _ ->
  Deferred.unit

let () =
  Command.async ~summary:"Kraken kdb+ compta" begin
    let open Command.Let_syntax in
    [%map_open
      let () = Logs_async_reporter.set_level_via_param [] in
      fun () ->
        Logs.set_reporter (Logs_async_reporter.reporter ()) ;
        main ()
    ] end |>
  Command.run
