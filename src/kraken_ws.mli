open Kraken

val url_public : Uri.t
(** For public channels only. *)

val url_auth : Uri.t
(** For public and private channels, auth is required in all cases. *)

val url_beta : Uri.t
(** Beta endpoint, do not use in prod. *)

type subscription = private
  | Ticker
  | OHLC of int
  | Trade
  | Book of int
  | Spread
  | All

  | OwnTrades of string
  | OpenOrders of string

type status = {
  connectionID: float ;
  status: string ;
  version: string
}

type subscribe = {
  reqid: int option ;
  pairs: Pair.t list ;
  sub: subscription ;
}

type subscription_status = {
  chanid : int option ;
  name : string ;
  pair : Pair.t option ;
  status : [`Subscribed|`Unsubscribed] ;
  reqid : int option ;
  subscription : subscription ;
} [@@deriving sexp_of]

type error = {
  event: string ;
  reqid : int option ;
  msg : string
} [@@deriving sexp_of]

type a = {
  price: float ;
  wholeLotVolume : int ;
  lotVolume : float
} [@@deriving sexp_of]

type b = {
  price: float ;
  wholeLotVolume : int ;
  lotVolume : float
} [@@deriving sexp_of]

type c = {
  price: float ;
  lotVolume : float
} [@@deriving sexp_of]

type v = {
  today: float ;
  last24Hours: float
} [@@deriving sexp_of]

type p = {
  today: float ;
  last24Hours: float
} [@@deriving sexp_of]

type ti = {
  today: int;
  last24Hours: int
} [@@deriving sexp_of]

type l = {
  today: float ;
  last24Hours: float
} [@@deriving sexp_of]

type h = {
  today: float ;
  last24Hours: float
} [@@deriving sexp_of]

type o = {
  today: float ;
  last24Hours: float
} [@@deriving sexp_of]

type ticker = {
  a: a ;
  b: b ;
  c: c ;
  v: v ;
  p: p ;
  t: ti;
  l: l ;
  h: h ;
  o: o ;
} [@@deriving sexp_of]

type trade = {
  price: float ;
  qty: float ;
  ts: Ptime.t ;
  side: Fixtypes.Side.t ;
  ord_type: Fixtypes.OrdType.t ;
  misc: string ;
} [@@deriving sexp_of]

type quote = {
  price: float ;
  qty: float ;
  ts: Ptime.t ;
  republish: bool ;
} [@@deriving sexp_of]

type book = {
  asks : quote list ;
  bids : quote list ;
} [@@deriving sexp_of]

type unsubscribe = {
  chanid : int ;
  reqid : int option
} [@@deriving sexp_of]

type t =
  | Error of error
  | Ping of float
  | Pong of float
  | HeartBt
  | Status of status
  | Subscribe of subscribe
  | Unsubscribe of unsubscribe
  | SubscriptionStatus of subscription_status
  | Ticker of ticker update
  | Trade of trade list update
  | Snapshot of book update
  | Quotes of book update

  | OwnTrades of Trade.t list
  | OpenOrders of Order.t list

and 'a update = {
  chanid: int ;
  feed: string ;
  pair: Pair.t ;
  data: 'a ;
}
[@@deriving sexp_of]

val ownTrades : ?reqid:int -> string -> t
val openOrders : ?reqid:int -> string -> t
val ping : float -> t
val tickers : ?reqid:int -> Pair.t list -> t
val trades : ?reqid:int -> Pair.t list -> t
val book10 : ?reqid:int -> Pair.t list -> t
val book25 : ?reqid:int -> Pair.t list -> t
val book100 : ?reqid:int -> Pair.t list -> t
val book500 : ?reqid:int -> Pair.t list -> t
val book1000 : ?reqid:int -> Pair.t list -> t

val pp : Format.formatter -> t -> unit
val encoding : t Json_encoding.encoding
