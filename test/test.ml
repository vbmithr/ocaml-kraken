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
    Fastrest.key = cfg.Cfg.key ;
    secret = Base64.decode_exn cfg.Cfg.secret ;
    meta = [] ;
  } in
  Alcotest_async.test_case n speed begin fun () ->
    Fastrest.request ~auth service >>= function
    | Ok _v -> Deferred.unit
    | Error e -> Error.raise e
  end

let rest = [
  wrap_request "time" time ;
  wrap_request "account_balance" account_balance ;
  wrap_request "trade_balance" trade_balance ;
  wrap_request "closed_orders" (closed_orders 0) ;
  wrap_request "trade_history" (trade_history 0) ;
  wrap_request "AssetPairs" asset_pairs ;
  wrap_request "ledgers" ledgers ;
  wrap_request "DepositMethodsEUR"  (deposit_methods ~asset:"EUR") ;
  wrap_request "DepositMethodsBTC"  (deposit_methods ~asset:"XBT") ;
  wrap_request "DepositMethodsXTZ"  (deposit_methods ~asset:"XTZ") ;
  wrap_request "DepositAddresses"  (deposit_addresses ~asset:"XBT" ~meth:"Bitcoin") ;
  wrap_request "DepositStatusXBT"  (deposit_status ~asset:"XBT" ~meth:"Bitcoin") ;
  wrap_request "DepositStatusXTZ"  (deposit_status ~asset:"XTZ" ~meth:"XTZ") ;
  (* wrap_request "WithdrawStatus"  (withdraw_status ~asset:"XBT" ~meth:"Bitcoin") ; *)
]

let () =
  Alcotest.run "kraken" [
    "rest", rest ;
  ]
