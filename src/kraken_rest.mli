open Async
open Httpaf
open Kraken

type get
type post

type _ meth =
  | Get : get meth
  | Post : post meth

type ('meth, 'a) service = {
  meth : 'meth meth ;
  url : Uri.t ;
  req : Request.t ;
  encoding: 'a Json_encoding.encoding ;
  pp : Format.formatter -> 'a -> unit ;
  params : (string * string list) list ;
}

type auth = {
  key : string ;
  secret : string ;
}

type error =
  | Http of Client_connection.error
  | Kraken of string list

val time : (get, Ptime.t) service

type 'a assoc = (string * 'a) list [@@deriving sexp]

val account_balance : (post, float assoc) service
val trade_balance : (post, Balance.t) service
val closed_orders : (post, Order.t assoc) service
val trade_history : (post, Filled_order.t assoc) service
val ledgers : (post, Ledger.t assoc) service

val request : ?auth:auth -> (_, 'a) service -> ('a, error) result Deferred.t
