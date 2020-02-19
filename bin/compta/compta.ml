open Core
open Async
open Kraken

let src = Logs.Src.create "kraken.compta"

module Log_async = (val Logs_async.src_log src : Logs_async.LOG)

let url = Uri.make ~scheme:"kdb" ~host:"localhost" ~port:5042 ()

let side_of_string = function "buy" -> Fixtypes.Side.Buy | _ -> Sell

let string_of_side = function Fixtypes.Side.Buy -> "buy" | Sell -> "sell"

let ordType_of_string = function
  | "limit" -> Fixtypes.OrdType.Limit
  | _ -> Market

let string_of_ordType = function
  | Fixtypes.OrdType.Limit -> "limit"
  | _ -> "market"

let floats = Kx.(v float)

let timestamps = Kx.(v timestamp)

let syms = Kx.(v sym)

let sides =
  Kx.(conv (Array.map ~f:string_of_side) (Array.map ~f:side_of_string) (v sym))

let ordTypes =
  Kx.(
    conv
      (Array.map ~f:string_of_ordType)
      (Array.map ~f:ordType_of_string)
      (v sym))

let krakids =
  Kx.(conv (Array.map ~f:KrakID.to_guid) (Array.map ~f:KrakID.of_guid) (v guid))

let trades =
  Kx.(
    t9 timestamps syms syms krakids krakids sides ordTypes (v float) (v float))

let kx_of_fills fills =
  let len = List.length fills in
  let times = Array.create ~len Ptime.epoch in
  let syms = Array.create ~len "" in
  let xchs = Array.create ~len "KRK" in
  let ids = Array.create ~len KrakID.zero in
  let oids = Array.create ~len KrakID.zero in
  let sides = Array.create ~len Fixtypes.Side.Buy in
  let ordTypes = Array.create ~len Fixtypes.OrdType.Market in
  let pxs = Array.create ~len Float.nan in
  let qties = Array.create ~len Float.nan in
  List.iteri fills
    ~f:(fun i
            { Trade.id; ordertxid; pair; time; side; ord_type; price; vol; _ }
            ->
      times.(i) <- time;
      syms.(i) <- pair;
      ids.(i) <- id;
      oids.(i) <- ordertxid;
      sides.(i) <- side;
      ordTypes.(i) <- ord_type;
      pxs.(i) <- price;
      qties.(i) <- vol);
  let open Kx in
  Kx_async.create
    (t3 (a sym) (a sym) trades)
    ( "upd",
      "trades",
      (times, syms, xchs, ids, oids, sides, ordTypes, pxs, qties) )

let ordersw =
  let open Kx in
  merge_tups
    (t10 timestamps syms syms krakids sides ordTypes floats floats floats
       floats)
    (t3 floats floats floats)

let kx_of_orders orders =
  let len = List.length orders in
  let times = Array.create ~len Ptime.epoch in
  let syms = Array.create ~len "" in
  let xchs = Array.create ~len "KRK" in
  let ids = Array.create ~len KrakID.zero in
  let sides = Array.create ~len Fixtypes.Side.Buy in
  let types = Array.create ~len Fixtypes.OrdType.Market in
  let qties = Array.create ~len Float.nan in
  let execQties = Array.create ~len Float.nan in
  let costs = Array.create ~len Float.nan in
  let fees = Array.create ~len Float.nan in
  let pxs = Array.create ~len Float.nan in
  let stopPxs = Array.create ~len Float.nan in
  let limitPxs = Array.create ~len Float.nan in
  List.iteri orders
    ~f:(fun i
            ({
               id;
               opentm;
               descr;
               vol;
               vol_exec;
               cost;
               fee;
               price;
               stopprice;
               limitprice;
               _;
             } :
              Order.t)
            ->
      times.(i) <- opentm;
      syms.(i) <- descr.pair;
      ids.(i) <- id;
      sides.(i) <- descr.side;
      types.(i) <- descr.ord_type;
      qties.(i) <- vol;
      execQties.(i) <- vol_exec;
      costs.(i) <- cost;
      costs.(i) <- cost;
      fees.(i) <- fee;
      pxs.(i) <- price;
      stopPxs.(i) <- (if Float.equal stopprice 0. then Kx.nf else stopprice);
      limitPxs.(i) <- (if Float.equal limitprice 0. then Kx.nf else limitprice));
  let open Kx in
  Kx_async.create
    (t3 (a sym) (a sym) ordersw)
    ( "upd",
      "orders",
      ( (times, syms, xchs, ids, sides, types, pxs, qties, execQties, costs),
        (fees, stopPxs, limitPxs) ) )

let ledgerTypes =
  Kx.(
    conv
      (fun v ->
        Array.map v
          ~f:(Fn.compose String.uncapitalize Kraken.Ledger.string_of_typ))
      (fun v ->
        Array.map v
          ~f:(Fn.compose Kraken.Ledger.typ_of_string String.capitalize))
      (v sym))

let ledgersw =
  let open Kx in
  t8 timestamps syms syms ledgerTypes krakids krakids floats floats

let kx_of_ledgers ledgers =
  let len = List.length ledgers in
  let times = Array.create ~len Ptime.epoch in
  let syms = Array.create ~len "" in
  let xchs = Array.create ~len "KRK" in
  let ids = Array.create ~len KrakID.zero in
  let refids = Array.create ~len KrakID.zero in
  let types = Array.create ~len Kraken.Ledger.Deposit in
  let amounts = Array.create ~len Float.nan in
  let fees = Array.create ~len Float.nan in
  List.iteri ledgers
    ~f:(fun i ({ asset; time; typ; id; refid; amount; fee; _ } : Ledger.t) ->
      times.(i) <- time;
      syms.(i) <- asset;
      ids.(i) <- id;
      types.(i) <- typ;
      refids.(i) <- refid;
      amounts.(i) <- amount;
      fees.(i) <- fee);
  let open Kx in
  Kx_async.create
    (t3 (a sym) (a sym) ledgersw)
    ("upd", "ledgers", (times, syms, xchs, types, ids, refids, amounts, fees))

let statuses =
  Kx.(
    conv
      (fun s -> Array.map s ~f:Kraken_rest.Transfer.string_of_status)
      (fun _ -> assert false)
      (v sym))

let transfersw =
  let open Kx in
  t10 timestamps syms syms ledgerTypes statuses krakids
    (list (s char))
    (list (s char))
    floats floats

let kx_of_transfers transfers kind =
  let len = List.length transfers in
  let times = Array.create ~len Ptime.epoch in
  let syms = Array.create ~len "" in
  let xchs = Array.create ~len "KRK" in
  let refids = Array.create ~len KrakID.zero in
  let txids = Array.create ~len "" in
  let addrs = Array.create ~len "" in
  let types = Array.create ~len Kraken.Ledger.Deposit in
  let statuses = Array.create ~len Kraken_rest.Transfer.Fail in
  let amounts = Array.create ~len Float.nan in
  let fees = Array.create ~len Float.nan in
  List.iteri transfers
    ~f:(fun i
            ({ asset; time; refid; txid; info; amount; fee; status; _ } :
              Kraken_rest.Transfer.t)
            ->
      refids.(i) <- refid;
      times.(i) <- time;
      syms.(i) <- asset;
      txids.(i) <- txid;
      types.(i) <- kind;
      addrs.(i) <- info;
      statuses.(i) <- status;
      amounts.(i) <- amount;
      fees.(i) <- fee);
  let open Kx in
  Kx_async.create
    (t3 (a sym) (a sym) transfersw)
    ( "upd",
      "transfers",
      (times, syms, xchs, types, statuses, refids, txids, addrs, amounts, fees)
    )

let key, secret =
  match String.split ~on:':' (Sys.getenv_exn "TOKEN_KRAKEN") with
  | [ key; secret ] -> (key, secret)
  | _ -> assert false

let auth = { Fastrest.key; secret = Base64.decode_exn secret; meta = [] }

let retrieveOrders w =
  let rec inner n =
    Fastrest.request ~auth (Kraken_rest.closed_orders ~ofs:n ()) >>= fun os ->
    Pipe.write w (kx_of_orders os) >>= fun () ->
    let len = List.length os in
    Logs_async.app (fun m -> m "Found %d closed orders" len) >>= fun () ->
    Deferred.List.iter os ~f:(fun os ->
        Log_async.app (fun m -> m "%a" Order.pp os))
    >>= fun () -> if len < 50 then Deferred.unit else inner (n + len)
  in
  inner 0

let retrieveFills w =
  let rec inner n =
    Fastrest.request ~auth (Kraken_rest.trade_history n) >>= fun fills ->
    Pipe.write w (kx_of_fills fills) >>= fun () ->
    let len = List.length fills in
    Logs_async.app (fun m -> m "Found %d fills" len) >>= fun () ->
    Deferred.List.iter fills ~f:(fun fill ->
        Log_async.app (fun m -> m "%a" Trade.pp fill))
    >>= fun () -> if len < 50 then Deferred.unit else inner (n + len)
  in
  inner 0

let retrieveLedgers w =
  let rec inner n =
    Fastrest.request ~auth (Kraken_rest.ledgers ~ofs:n ()) >>= fun ledgers ->
    Pipe.write w (kx_of_ledgers ledgers) >>= fun () ->
    let len = List.length ledgers in
    Logs_async.app (fun m -> m "Found %d ledger entries" len) >>= fun () ->
    Deferred.List.iter ledgers ~f:(fun l ->
        Log_async.app (fun m -> m "%a" Ledger.pp l))
    >>= fun () -> if len < 50 then Deferred.unit else inner (n + len)
  in
  inner 0

let retrieveTransfers w asset meth =
  Fastrest.request ~auth (Kraken_rest.transfer_status ~asset ~meth `Deposit)
  >>= fun deposits ->
  Fastrest.request ~auth (Kraken_rest.transfer_status ~asset ~meth `Withdrawal)
  >>= fun withdrawals ->
  Pipe.write w (kx_of_transfers deposits Deposit) >>= fun () ->
  Pipe.write w (kx_of_transfers withdrawals Withdrawal) >>= fun () ->
  let nbDeposits = List.length deposits in
  let nbWithdrawals = List.length withdrawals in
  Logs_async.app (fun m ->
      m "Found %d/%d %s deposits" nbDeposits nbWithdrawals asset)
  >>= fun () ->
  Deferred.List.iter deposits ~f:(fun d ->
      Log_async.app (fun m -> m "%a" Kraken_rest.Transfer.pp d))

let main () =
  Kx_async.with_connection url (fun { w; _ } ->
      retrieveTransfers w "XTZ" "XTZ" >>= fun () ->
      retrieveTransfers w "BTC" "Bitcoin" >>= fun () ->
      retrieveOrders w >>= fun () ->
      retrieveFills w >>= fun () -> retrieveLedgers w)
  |> Deferred.ignore_m

let () =
  Command.async ~summary:"Kraken kdb+ compta"
    (let open Command.Let_syntax in
    [%map_open
      let () = Logs_async_reporter.set_level_via_param [] in
      (* and ledger = flag "ledger" no_arg ~doc:"Record ledger entries in DB" in *)
      fun () ->
        Logs.set_reporter (Logs_async_reporter.reporter ());
        main ()])
  |> Command.run
