open Core
open Async

open Kraken_rest

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

let () =
  Logs.set_reporter (Logs_async_reporter.reporter ()) ;
  Logs.set_level (Some Debug)

let wrap_request ?(speed=`Quick) n service =
  let auth = {
    key = cfg.key ;
    secret = Base64.decode_exn cfg.secret ;
  } in
  Alcotest_async.test_case n speed begin fun () ->
    request ~auth service >>= function
    | Ok v ->
      Logs_async.info (fun m -> m "%a" service.pp v) ;
    | Error _ -> failwith ""
  end

let rest = [
  (* wrap_request "time" time ;
   * wrap_request "account_balance" account_balance ;
   * wrap_request "trade_balance" trade_balance ; *)
  (* wrap_request "closed_orders" closed_orders ;
   * wrap_request "trade_history" trade_history ; *)
  wrap_request "ledgers" ledgers ;
]

let () =
  Alcotest.run "kraken" [
    "rest", rest ;
  ]
