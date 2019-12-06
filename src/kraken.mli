open Json_encoding

val strfloat : float encoding

module Ezjsonm_encoding : sig
  include module type of Json_encoding.Make(Json_repr.Ezjsonm)
  val destruct_safe : 'a encoding -> Ezjsonm.value -> 'a
end

module Ptime : sig
  include module type of Ptime
    with type t = Ptime.t
     and type span = Ptime.span
     and type date = Ptime.date

  val t_of_sexp : Sexplib.Sexp.t -> Ptime.t
  val sexp_of_t : Ptime.t -> Sexplib.Sexp.t
  val date_of_sexp : Sexplib.Sexp.t -> date
  val sexp_of_date : date -> Sexplib.Sexp.t
  val encoding : t encoding
end

module OrdType : sig
  type t = Fixtypes.OrdType.t [@@deriving sexp]
  val encoding : t encoding
end

module OrdStatus : sig
  type t = Fixtypes.OrdStatus.t
  val encoding : t encoding
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
  val encoding : t encoding
end

module Order : sig
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
  val encoding : t encoding
end

module Filled_order : sig
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

  val pp : Format.formatter -> t -> unit
  val encoding : t encoding
end

type aclass = [`currency]
val aclass : aclass encoding

module Ledger : sig
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

  val pp : Format.formatter -> t -> unit
  val encoding : t encoding
end

module Pair : sig
  type t = {
    altname: string ;
    wsname: string option ;
    aclass_base: aclass ;
    base: string ;
    aclass_quote: aclass ;
    quote: string ;
    pair_decimals: int ;
  } [@@deriving sexp]

  val pp : Format.formatter -> t -> unit
  val encoding : t encoding
end
