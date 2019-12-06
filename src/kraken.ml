open Sexplib.Std
open Json_encoding

let strfloat =
  union [
    case float (fun s -> Some s) (fun s -> s) ;
    case string (fun s -> Some (string_of_float s)) float_of_string ;
  ]

let side_encoding =
  string_enum [
    "buy", Fixtypes.Side.Buy ;
    "sell", Sell ;
  ]

module Ezjsonm_encoding = struct
  include Json_encoding.Make(Json_repr.Ezjsonm)

  let destruct_safe encoding value =
    try destruct encoding value with exn ->
      Format.eprintf "%a@."
        (Json_encoding.print_error ?print_unknown:None) exn ;
      raise exn
end

module Ptime = struct
  include Ptime

  type date_ = int * int * int [@@deriving sexp]

  let date_of_sexp = date__of_sexp
  let sexp_of_date = sexp_of_date_

  let t_of_sexp sexp =
    let sexp_str = string_of_sexp sexp in
    match of_rfc3339 sexp_str with
    | Ok (t, _, _) -> t
    | _ -> invalid_arg "Ptime.t_of_sexp"

  let sexp_of_t t =
    sexp_of_string (to_rfc3339 t)

  let encoding =
    conv
      Ptime.to_float_s
      (fun ts -> match Ptime.of_float_s ts with
         | None -> invalid_arg "Ptime.encoding"
         | Some ts -> ts)
      strfloat
end

module OrdType = struct
  type t = Fixtypes.OrdType.t [@@deriving sexp]

  let encoding =
    string_enum [
      "market", Fixtypes.OrdType.Market ;
      "limit", Limit ;
      "stop-loss", Stop ;
      "stop-loss-limit", StopLimit ;
      "take-profit", MarketIfTouched ;
    ]
end

module OrdStatus = struct
  type t = Fixtypes.OrdStatus.t [@@deriving sexp]

  let encoding =
    string_enum [
      "pending", Fixtypes.OrdStatus.PendingNew ;
      "open", New ;
      "closed", Filled ;
      "canceled", Canceled ;
      "expired", Canceled ;
    ]
end

module Balance = struct
  type t = {
    equivalent_balance : float ;
    trade_balance : float ;
    total_margin : float ;
    pnl : float ;
    positions_cost : float ;
    positions_value : float ;
    equity : float ;
    free_margin : float ;
  } [@@deriving sexp]

  let pp ppf t =
    Format.fprintf ppf "%a" Sexplib.Sexp.pp (sexp_of_t t)

  let encoding =
    conv
      (fun { equivalent_balance ;
             trade_balance ; total_margin ;
             pnl ; positions_cost ; positions_value ;
             equity ; free_margin } ->
        (equivalent_balance, trade_balance,
         total_margin, pnl, positions_cost, positions_value,
         equity, free_margin))
      (fun (equivalent_balance, trade_balance,
            total_margin, pnl, positions_cost, positions_value,
            equity, free_margin) ->
        { equivalent_balance ;
          trade_balance ; total_margin ;
          pnl ; positions_cost ; positions_value ;
          equity ; free_margin })
      (obj8
         (req "eb" strfloat)
         (req "tb" strfloat)
         (req "m" strfloat)
         (req "n" strfloat)
         (req "c" strfloat)
         (req "v" strfloat)
         (req "e" strfloat)
         (req "mf" strfloat))
end

module Order = struct
  type descr = {
    pair: string ;
    side: Fixtypes.Side.t ;
    ord_type: OrdType.t ;
    price: float ;
    price2: float ;
    leverage: string ;
    order: string ;
    close: string option ;
  } [@@deriving sexp]

  let descr_encoding =
    conv
      (fun { pair ; side ; ord_type ; price ;
             price2 ; leverage ; order ; close } ->
        (pair, side, ord_type, price,
         price2, leverage, order, close))
      (fun (pair, side, ord_type, price,
            price2, leverage, order, close) ->
        { pair ; side ; ord_type ; price ;
          price2 ; leverage ; order ; close })
      (obj8
         (req "pair" string)
         (req "type" side_encoding)
         (req "ordertype" OrdType.encoding)
         (req "price" strfloat)
         (req "price2" strfloat)
         (req "leverage" string)
         (req "order" string)
         (opt "close" string))

  type t = {
    status: OrdStatus.t ;
    opentm: Ptime.t option ;
    closetm: Ptime.t option ;
    starttm: Ptime.t option ;
    expiretm: Ptime.t option ;
    descr: descr;
    vol: float ;
    vol_exec: float ;
    cost: float ;
    fee: float ;
    price: float ;
    stopprice: float option ;
    limitprice: float option ;
    misc: string ;
    oflags: string ;
  } [@@deriving sexp]

  let pp ppf t =
    Format.fprintf ppf "%a" Sexplib.Sexp.pp (sexp_of_t t)

  let otherfields_encoding =
    obj4
      (req "descr" descr_encoding)
      (req "status" OrdStatus.encoding)
      (req "misc" string)
      (req "oflags" string)

  let times_encoding =
    obj4
      (opt "opentm" Ptime.encoding)
      (opt "closetm" Ptime.encoding)
      (opt "starttm" Ptime.encoding)
      (opt "expiretm" Ptime.encoding)

  let floats_encoding =
    obj7
      (req "vol" strfloat)
      (req "vol_exec" strfloat)
      (req "cost" strfloat)
      (req "fee" strfloat)
      (req "price" strfloat)
      (opt "stopprice" strfloat)
      (opt "limitprice" strfloat)

  let encoding =
    conv
      (fun { status ; opentm ; closetm ; starttm ; expiretm ;
             descr ; vol ; vol_exec ; cost ; fee ; price ;
             stopprice ; limitprice ; misc ; oflags
           } ->
        (((), (descr, status, misc, oflags)),
         (opentm, closetm, starttm, expiretm)),
        (vol, vol_exec, cost, fee, price, stopprice, limitprice))
      (fun ((((), (descr, status, misc, oflags)),
             (opentm, closetm, starttm, expiretm)),
            (vol, vol_exec, cost, fee, price, stopprice, limitprice)) ->
        { status ; opentm ; closetm ; starttm ; expiretm ;
          descr ; vol ; vol_exec ; cost ; fee ; price ;
          stopprice ; limitprice ; misc ; oflags
        })
      (merge_objs
         (merge_objs
            (merge_objs unit otherfields_encoding)
            times_encoding)
         floats_encoding)
end

module Filled_order = struct
  type t = {
    ordertxid: string ;
    postxid: string option ;
    pair: string ;
    time: Ptime.t ;
    side: Fixtypes.Side.t ;
    ord_type: OrdType.t ;
    price: float ;
    cost: float ;
    fee: float ;
    vol: float ;
    margin: float ;
    misc: string ;
  } [@@deriving sexp]

  let pp ppf t =
    Format.fprintf ppf "%a" Sexplib.Sexp.pp (sexp_of_t t)

  let encoding =
    conv
      (fun { ordertxid ; postxid ; pair ; time ; side ; ord_type ;
             price ; cost ; fee ; vol ; margin ; misc } ->
        (ordertxid, pair, time, side, ord_type, price, cost, fee, vol, margin), (misc, postxid))
      (fun ((ordertxid, pair, time, side, ord_type, price, cost, fee, vol, margin), (misc, postxid)) ->
         { ordertxid ; postxid ; pair ; time ; side ; ord_type ;
           price ; cost ; fee ; vol ; margin ; misc })
      (merge_objs
         (obj10
            (req "ordertxid" string)
            (req "pair" string)
            (req "time" Ptime.encoding)
            (req "type" side_encoding)
            (req "ordertype" OrdType.encoding)
            (req "price" strfloat)
            (req "cost" strfloat)
            (req "fee" strfloat)
            (req "vol" strfloat)
            (req "margin" strfloat))
         (obj2
            (req "misc" string)
            (opt "postxid" string)))
end

type aclass = [`currency] [@@deriving sexp]

let aclass =
  string_enum ["currency", `currency]

module Ledger = struct
  type t = {
    refid : string ;
    time : Ptime.t ;
    typ : [`deposit|`withdrawal|`trade|`margin|`transfer] ;
    aclass : aclass ;
    asset : string ;
    amount : float ;
    fee : float ;
    balance : float ;
  } [@@deriving sexp]

  let pp ppf t =
    Format.fprintf ppf "%a" Sexplib.Sexp.pp (sexp_of_t t)

  let typ_encoding =
    string_enum [
      "deposit", `deposit ;
      "withdrawal", `withdrawal ;
      "trade", `trade ;
      "margin", `margin ;
      "transfer", `transfer ;
    ]

  let encoding =
    conv
      (fun { refid ; time ; typ ; aclass ; asset ; amount ; fee ; balance } ->
         (refid, time, typ, aclass, asset, amount, fee, balance))
      (fun (refid, time, typ, aclass, asset, amount, fee, balance) ->
         { refid ; time ; typ ; aclass ; asset ; amount ; fee ; balance })
      (obj8
         (req "refid" string)
         (req "time" Ptime.encoding)
         (req "type" typ_encoding)
         (req "aclass" aclass)
         (req "asset" string)
         (req "amount" strfloat)
         (req "fee" strfloat)
         (req "balance" strfloat))
end

module Pair = struct
  type t = {
    altname: string ;
    wsname: string option ;
    aclass_base: aclass ;
    base: string ;
    aclass_quote: aclass ;
    quote: string ;
    pair_decimals: int ;
  } [@@deriving sexp]

  let pp ppf t =
    Format.fprintf ppf "%a" Sexplib.Sexp.pp (sexp_of_t t)

  let encoding =
    conv
      (fun { altname ; wsname ; aclass_base ; base ; aclass_quote ; quote ; pair_decimals } ->
         ((), (altname, wsname, aclass_base, base, aclass_quote, quote, pair_decimals)))
      (fun ((), (altname, wsname, aclass_base, base, aclass_quote, quote, pair_decimals)) ->
         { altname ; wsname ; aclass_base ; base ; aclass_quote ; quote ; pair_decimals })
      (merge_objs unit
         (obj7
            (req "altname" string)
            (opt "wsname" string)
            (req "aclass_base" aclass)
            (req "base" string)
            (req "aclass_quote" aclass)
            (req "quote" string)
            (req "pair_decimals" int)))
end
