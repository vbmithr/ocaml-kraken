open Core
open Async
open Alcotest

open Kraken
open Kraken_rest

let key, secret =
  match String.split ~on:':' (Sys.getenv_exn "TOKEN_KRAKEN") with
  | [key; secret] -> key, secret
  | _ -> assert false

let wrap_request ?(speed=`Quick) n service =
  let auth = {
    Fastrest.key ;
    secret = Base64.decode_exn secret ;
    meta = [] ;
  } in
  Alcotest_async.test_case n speed begin fun () ->
    Deferred.ignore_m (Fastrest.request ~auth service)
  end

let partial_rt n =
  let open KrakID in
  let s' = Bytes.create 8 in
  for _ = 0 to n do
    let s = String.init 7 ~f:(fun _ -> chr (Random.int 36)) in
    let i = int_of_4 s 3 in
    chars_of_int s' 3 i ;
    let s = String.sub s ~pos:3 ~len:4 in
    check string s s Bytes.(unsafe_to_string ~no_mutation_while_string_reachable:(sub s' ~pos:3 ~len:4))
  done

let uuid_testable = testable Uuidm.pp Uuidm.equal

let uuid n =
  for _ = 0 to n - 1 do
    let u = Uuidm.create `V4 in
    let u' = KrakID.(to_guid (of_guid u)) in
    check uuid_testable (Uuidm.to_string u) u u'
  done

let roundtrip ss =
  List.iter ss ~f:begin fun s ->
    let s' = KrakID.(to_string (of_string s)) in
    check string s s s'
  end

let krakid = [
  Alcotest.test_case "basic" `Quick (fun () -> partial_rt 10000) ;
  Alcotest.test_case "uuid" `Quick (fun () -> uuid 10000) ;
  Alcotest.test_case "roundtrip" `Quick (fun () -> roundtrip ["BMBSASZ-E3ZQEE-JB3US4";
                                                              "L7V3M2-CH7MR-WMPRR7";
                                                              "L7V3M2-CH7MR-WMPRR7";
                                                              "RMB4MTB-O5XAS-2EWPSX"])
]

let rest = [
  wrap_request "time" time ;
  wrap_request "assets" assets ;
  wrap_request "symbols" symbols ;
  wrap_request "account_balance" account_balance ;
  wrap_request "trade_balance" (trade_balance ()) ;
  wrap_request "trade_balanceXBT" (trade_balance ~asset:"XBT" ()) ;
  wrap_request "closed_orders" (closed_orders ~ofs:0 ()) ;
  wrap_request "trade_history" (trade_history 0) ;
  wrap_request "ledgers" (ledgers ()) ;
  (* wrap_request "DepositMethodsEUR"  (deposit_methods ~asset:"EUR") ; *)
  wrap_request "DepositMethodsBTC"  (deposit_methods ~asset:"XBT") ;
  (* wrap_request "DepositMethodsXTZ"  (deposit_methods ~asset:"XTZ") ; *)
  wrap_request "DepositAddresses"  (deposit_addresses ~asset:"XBT" ~meth:"Bitcoin") ;
  wrap_request "DepositStatusXBT"  (transfer_status ~asset:"XBT" ~meth:"Bitcoin" `Deposit) ;
  wrap_request "DepositStatusXTZ"  (transfer_status ~asset:"XTZ" ~meth:"XTZ" `Deposit) ;
  wrap_request "WithdrawStatus"  (transfer_status ~asset:"XBT" ~meth:"Bitcoin" `Withdrawal) ;
  wrap_request "WithdrawStatus"  (transfer_status ~asset:"XTZ" ~meth:"XTZ" `Withdrawal) ;
  wrap_request "WebsocketToken" websocket_token ;
]

let () =
  Logs.set_reporter (Logs_async_reporter.reporter ()) ;
  Logs.set_level (Some Debug) ;
  Alcotest.run "kraken" [
    "krakid", krakid ;
    "rest", rest ;
  ]
