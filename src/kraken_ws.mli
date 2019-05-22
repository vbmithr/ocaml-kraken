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
  status : subscriptionStatus ;
  pair : Pair.t ;
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
  side: [`buy|`sell] ;
  ord_type: [`market|`limit] ;
  misc: string ;
} [@@deriving sexp]

type book_entry = {
  price: float ;
  qty: float ;
  ts: Ptime.t ;
} [@@deriving sexp]

type book = {
  asks : book_entry list ;
  bids : book_entry list ;
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
  | Trade of int * trade list
  | Snapshot of int * book
  | BookUpdate of int * book
[@@deriving sexp]

val pp : Format.formatter -> t -> unit
val encoding : t Json_encoding.encoding
