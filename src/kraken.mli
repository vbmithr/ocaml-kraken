module Ezjsonm_encoding : sig
  include module type of Json_encoding.Make(Json_repr.Ezjsonm)
  val destruct_safe : 'a Json_encoding.encoding -> Ezjsonm.value -> 'a
end

module Ptime : sig
  include module type of Ptime
    with type t = Ptime.t
     and type span = Ptime.span

  val t_of_sexp : Sexplib.Sexp.t -> Ptime.t
  val sexp_of_t : Ptime.t -> Sexplib.Sexp.t
end

module OrdType : sig
  type t = [
    | `order_type_unset
    | `order_type_market
    | `order_type_limit
    | `order_type_stop
    | `order_type_stop_limit
    | `order_type_market_if_touched
  ]

  val encoding : t Json_encoding.encoding
end

module OrdStatus : sig
  type t = [
      | `order_status_pending_open
      | `order_status_open
      | `order_status_filled
      | `order_status_canceled
    ]

  val encoding : t Json_encoding.encoding
end

module Balance : sig
  type t = {
    equivalent_balance : float ;
    trade_balance : float ;
    total_margin : float ;
    pnl : float ;
    positions_cost : float ;
    positions_value : float ;
    equity : float ;
    free_margin : float ;
    margin_level : float ;
  }
end

module Order : sig
  type descr = {
    pair: string;
    (* type_: string [@key "type"]; *)
    ordertype: string;
    price: string;
    price2: string;
    leverage: string;
    order: string;
    (* close: (string [@default ""]); *)
  }

  type t = {
    refid: string option;
    userref: string option;
    status: string;
    opentm: float;
    starttm: float;
    expiretm: float;
    descr: descr;
    vol: string;
    vol_exec: string;
    cost: string;
    fee: string;
    price: string;
    (* stopprice: (string [@default ""]); *)
    (* limitprice: (string [@default ""]); *)
    (* misc: string; *)
    (* oflags: string; *)
    (* trades: (string list [@default []]); *)
  }
end

module Filled_order : sig
  type t = {
    ordertxid: string;
    pair: string;
    time: float;
    type_: string;
    ordertype: string;
    price: string;
    cost: string;
    fee: string;
    vol: string;
    margin: string;
    misc: string;
    (* closing: *)
    (* posstatus: *)
    (* cprice: *)
    (* ccost: *)
    (* cfee: *)
    (* cvol: *)
    (* cmargin: *)
    (* net: *)
    (* trades: *)
  }
end
