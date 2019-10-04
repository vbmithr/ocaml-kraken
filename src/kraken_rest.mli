open Kraken
open Fastrest

val time : (form, Ptime.t) service

type 'a assoc = (string * 'a) list [@@deriving sexp]

val asset_pairs : (form, (string * Pair.t) list) service
val account_balance : (form, float assoc) service
val trade_balance : (form, Balance.t) service
val closed_orders : int -> (form, Order.t assoc) service
val trade_history : int -> (form, Filled_order.t assoc) service
val ledgers : (form, Ledger.t assoc) service
