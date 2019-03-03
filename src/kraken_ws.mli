type subscription =
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
  pair: string list ;
  sub: subscription ;
}

type subscriptionStatus =
  | Subscribed
  | Unsubscribed
  | Error of string

type subscription_status = {
  chanid : int ;
  status : subscriptionStatus ;
  pair : string ;
  reqid : int option ;
  subscription : subscription ;
}

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
}

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
