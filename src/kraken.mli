val strfloat : float Json_encoding.encoding

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
  val encoding : t Json_encoding.encoding
end

module OrdType : sig
  type t = [
    | `order_type_unset
    | `order_type_market
    | `order_type_limit
    | `order_type_stop
    | `order_type_stop_limit
    | `order_type_market_if_touched
  ] [@@deriving sexp]

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
  } [@@deriving sexp]

  val pp : Format.formatter -> t -> unit
  val encoding : t Json_encoding.encoding
end

module Order : sig
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

  val pp : Format.formatter -> t -> unit
  val encoding : t Json_encoding.encoding
end

module Filled_order : sig
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

  val pp : Format.formatter -> t -> unit
  val encoding : t Json_encoding.encoding
end

module Ledger : sig
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

  val pp : Format.formatter -> t -> unit
  val encoding : t Json_encoding.encoding
end
