open Json_encoding

module Pair : sig
  type t = {
    base: string ;
    quote: string ;
  }  [@@deriving sexp]

  val compare : t -> t -> int

  val pp : Format.formatter -> t -> unit
  val to_string : t -> string
  val of_string : string -> t option
  val of_string_exn : string -> t
  val encoding : t Json_encoding.encoding

  module Set : Set.S with type elt := t
  module Map : Map.S with type key := t
  module Table : Hashtbl.S with type key := t
end

module KrakID : sig
  type kind =
    | Order
    | Deposit
    | Withdrawal
    | Trade
    | Ledger

  type t [@@deriving sexp_of]

  val zero : t
  val kind : t -> kind
  val of_string : string -> t
  val to_string : t -> string
  val pp : t Fmt.t
  val encoding : t encoding
  val of_guid : Uuidm.t -> t
  val to_guid : t -> Uuidm.t

  (**/**)
  val idx : char -> int
  val chr : int -> char
  val int_of_4 : string -> int -> int
  val chars_of_int : bytes -> int -> int -> unit
end

val strfloat : float encoding
val kraklist : ('a -> 'b encoding) -> (string -> 'a) -> 'b list encoding

module Ezjsonm_encoding : sig
  include module type of Json_encoding.Make(Json_repr.Ezjsonm)
  val destruct_safe : 'a encoding ->
    Json_repr.Ezjsonm.value -> 'a
end

module Ptime : sig
  include module type of Ptime
    with type t = Ptime.t
     and type span = Ptime.span
     and type date = Ptime.date

  val sexp_of_t : Ptime.t -> Sexplib.Sexp.t
  val sexp_of_date : date -> Sexplib.Sexp.t
  val encoding : t encoding
end

module OrdType : sig
  type t = Fixtypes.OrdType.t [@@deriving sexp_of]
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
  } [@@deriving sexp_of]

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
  } [@@deriving sexp_of]

  type t = {
    id: KrakID.t ;
    status: OrdStatus.t ;
    opentm: Ptime.t ;
    closetm: Ptime.t ;
    starttm: Ptime.t ;
    expiretm: Ptime.t ;
    descr: descr;
    vol: float ;
    vol_exec: float ;
    cost: float ;
    fee: float ;
    price: float ;
    stopprice: float ;
    limitprice: float ;
    misc: string ;
    oflags: string ;
  } [@@deriving sexp_of]

  val pp : Format.formatter -> t -> unit
  val encoding : KrakID.t -> t encoding
end

module Trade : sig
  type t = {
    id: KrakID.t ;
    ordertxid: KrakID.t ;
    postxid: KrakID.t option ;
    pair: string ;
    time: Ptime.t ;
    side: Fixtypes.Side.t ;
    ord_type: OrdType.t ;
    price: float ;
    cost: float ;
    fee: float ;
    vol: float ;
    margin: float ;
    misc: string option ;
  } [@@deriving sexp_of]

  val pp : Format.formatter -> t -> unit
  val encoding : KrakID.t -> t encoding
end

type aclass = [`currency] [@@deriving sexp_of]
val aclass : aclass encoding

module Ledger : sig
  type typ =
    | Deposit
    | Withdrawal
    | Trade
    | Margin
    | Transfer [@@deriving sexp_of]

  val string_of_typ : typ -> string
  val typ_of_string : string -> typ

  type t = {
    id: KrakID.t ;
    refid : KrakID.t ;
    time : Ptime.t ;
    typ : typ ;
    aclass : aclass ;
    asset : string ;
    amount : float ;
    fee : float ;
    balance : float ;
  } [@@deriving sexp_of]

  val pp : Format.formatter -> t -> unit
  val encoding : KrakID.t -> t encoding
end
