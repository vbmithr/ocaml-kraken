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

let side_of_string = function "buy" -> `buy | _ -> `sell
let string_of_side = function `buy -> "buy" | `sell -> "sell"

let ordType_of_string = function "limit" -> `order_type_limit | _ -> `order_type_market
let string_of_ordType = function `order_type_limit -> "limit" | #Kraken.OrdType.t -> "market"

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
  t7 (v timestamp) (v sym) (compounds char) sides_encoding ordTypes_encoding
    (v float) (v float)

let insertFills w fills =
  let open Kx in
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
  let v =
    construct line Array.(of_list times,
                          of_list syms,
                          of_list tids,
                          of_list sides,
                          of_list ordTypes,
                          of_list prices,
                          of_list qties) in
  Pipe.write w ("upd", [|v|])

let main () =
  Kx_async.with_connection url ~f:begin fun _r w ->
    let rec inner n =
      Fastrest.request
        ~auth:{ Fastrest.key = cfg.key ;
                secret = Base64.decode_exn cfg.secret ;
                meta = [] } (Kraken_rest.trade_history n) >>= function
      | Error e ->
        Log_async.err begin fun m ->
          m "%a" (Fastrest.pp_print_error Format.(pp_print_list pp_print_string)) e
        end
      | Ok fills ->
        insertFills w fills >>= fun () ->
        let len = List.length fills in
        Logs_async.app (fun m -> m "Found %d fills" len) >>= fun () ->
        Deferred.List.iter fills ~f:begin fun (str, fill) ->
          Log_async.app (fun m -> m "%s %a" str Filled_order.pp fill)
        end >>= fun () ->
        if len < 50 then Deferred.unit
        else inner (n + len) in
    inner 0
  end >>= fun _ ->
  Deferred.unit

let () =
  Command.async ~summary:"Kraken kdb+ compta" begin
    let open Command.Let_syntax in
    [%map_open
      let () = Logs_async_reporter.set_level_via_param None in
      fun () ->
        Logs.set_reporter (Logs_async_reporter.reporter ()) ;
        main ()
    ] end |>
  Command.run
