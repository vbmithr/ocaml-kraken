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
end

type subscription = private
  | Ticker
  | OHLC of int
  | Trade
  | Book of int
  | Spread
  | All

val subscription_encoding : subscription Json_encoding.encoding

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

val trades : ?reqid:int -> Pair.t list -> subscribe
val book10 : ?reqid:int -> Pair.t list -> subscribe
val book25 : ?reqid:int -> Pair.t list -> subscribe
val book100 : ?reqid:int -> Pair.t list -> subscribe
val book500 : ?reqid:int -> Pair.t list -> subscribe
val book1000 : ?reqid:int -> Pair.t list -> subscribe

type subscriptionStatus =
  | Subscribed
  | Unsubscribed
  | Error of string

type subscription_status = {
  chanid : int ;
  name : string ;
  pair : Pair.t ;
  status : subscriptionStatus ;
  reqid : int option ;
  subscription : subscription ;
} [@@deriving sexp]

type error = {
  reqid : int option ;
  msg : string
} [@@deriving sexp]

type trade = {
  price: float ;
  qty: float ;
  ts: Ptime.t ;
  side: [`Buy | `Sell] ;
  ord_type: [`Market | `Limit] ;
  misc: string ;
} [@@deriving sexp]

type quote = {
  price: float ;
  qty: float ;
  ts: Ptime.t ;
  republish: bool ;
} [@@deriving sexp]

type book = {
  asks : quote list ;
  bids : quote list ;
} [@@deriving sexp]

type unsubscribe = {
  chanid : int ;
  reqid : int option
} [@@deriving sexp]

type t =
  | Ping of int option
  | Pong of int option
  | HeartBt
  | Status of status
  | Subscribe of subscribe
  | Unsubscribe of unsubscribe
  | Error of error
  | SubscriptionStatus of subscription_status
  | Trade of trade list update
  | Snapshot of book update
  | Quotes of book update

and 'a update = {
  chanid: int ;
  feed: string ;
  pair: Pair.t ;
  data: 'a ;
}
[@@deriving sexp]


val pp : Format.formatter -> t -> unit
val encoding : t Json_encoding.encoding
