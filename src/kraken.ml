open Sexplib.Std

let strfloat =
  let open Json_encoding in
  union [
    case float (fun s -> Some s) (fun s -> s) ;
    case string (fun s -> Some (string_of_float s)) float_of_string ;
  ]

let side_encoding =
  let open Json_encoding in
  string_enum [
    "buy", `buy ;
    "sell", `sell ;
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

  let t_of_sexp sexp =
    let sexp_str = string_of_sexp sexp in
    match of_rfc3339 sexp_str with
    | Ok (t, _, _) -> t
    | _ -> invalid_arg "Ptime.t_of_sexp"

  let sexp_of_t t =
    sexp_of_string (to_rfc3339 t)

  let encoding =
    let open Json_encoding in
    conv
      Ptime.to_float_s
      (fun ts -> match Ptime.of_float_s ts with
         | None -> invalid_arg "Ptime.encoding"
         | Some ts -> ts)
      strfloat
end

module OrdType = struct
  type t = [
    | `order_type_unset
    | `order_type_market
    | `order_type_limit
    | `order_type_stop
    | `order_type_stop_limit
    | `order_type_market_if_touched
  ]  [@@deriving sexp]

  let encoding =
    let open Json_encoding in
    string_enum [
      "market", `order_type_market ;
      "limit", `order_type_limit ;
      "stop-loss", `order_type_stop ;
      "stop-loss-limit", `order_type_stop_limit ;
      "take-profit", `order_type_market_if_touched ;
    ]
end

module OrdStatus = struct
  type t = [
    | `order_status_pending_open
    | `order_status_open
    | `order_status_filled
    | `order_status_canceled
  ] [@@deriving sexp]

  let encoding =
    let open Json_encoding in
    string_enum [
      "pending", `order_status_pending_open ;
      "open", `order_status_open ;
      "closed", `order_status_filled ;
      "canceled", `order_status_canceled ;
      "expired", `order_status_canceled ;
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
    let open Json_encoding in
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
    side: [`buy | `sell] ;
    ord_type: OrdType.t ;
    price: float ;
    price2: float ;
    leverage: string ;
    order: string ;
    close: string option ;
  } [@@deriving sexp]

  let descr_encoding =
    let open Json_encoding in
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
    let open Json_encoding in
    obj4
      (req "descr" descr_encoding)
      (req "status" OrdStatus.encoding)
      (req "misc" string)
      (req "oflags" string)

  let times_encoding =
    let open Json_encoding in
    obj4
      (opt "opentm" Ptime.encoding)
      (opt "closetm" Ptime.encoding)
      (opt "starttm" Ptime.encoding)
      (opt "expiretm" Ptime.encoding)

  let floats_encoding =
    let open Json_encoding in
    obj7
      (req "vol" strfloat)
      (req "vol_exec" strfloat)
      (req "cost" strfloat)
      (req "fee" strfloat)
      (req "price" strfloat)
      (opt "stopprice" strfloat)
      (opt "limitprice" strfloat)

  let encoding =
    let open Json_encoding in
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
    side: [`buy | `sell] ;
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
    let open Json_encoding in
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

module Ledger = struct
  type t = {
    refid : string ;
    time : Ptime.t ;
    typ : [`deposit|`withdrawal|`trade|`margin|`transfer] ;
    aclass : [`currency] ;
    asset : string ;
    amount : float ;
    fee : float ;
    balance : float ;
  } [@@deriving sexp]

  let pp ppf t =
    Format.fprintf ppf "%a" Sexplib.Sexp.pp (sexp_of_t t)

  let typ_encoding =
    let open Json_encoding in
    string_enum [
      "deposit", `deposit ;
      "withdrawal", `withdrawal ;
      "trade", `trade ;
      "margin", `margin ;
      "transfer", `transfer ;
    ]

  let aclass_encoding =
    Json_encoding.string_enum ["currency", `currency]

  let encoding =
    let open Json_encoding in
    conv
      (fun { refid ; time ; typ ; aclass ; asset ; amount ; fee ; balance } ->
         (refid, time, typ, aclass, asset, amount, fee, balance))
      (fun (refid, time, typ, aclass, asset, amount, fee, balance) ->
         { refid ; time ; typ ; aclass ; asset ; amount ; fee ; balance })
      (obj8
         (req "refid" string)
         (req "time" Ptime.encoding)
         (req "type" typ_encoding)
         (req "aclass" aclass_encoding)
         (req "asset" string)
         (req "amount" strfloat)
         (req "fee" strfloat)
         (req "balance" strfloat))
end
