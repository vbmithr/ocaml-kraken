open Kraken
open Fastrest

val time : (form, Ptime.t, string list) service

type 'a assoc = (string * 'a) list [@@deriving sexp]

val asset_pairs : (form, (string * Pair.t) list, string list) service
val account_balance : (form, float assoc, string list) service
val trade_balance : (form, Balance.t, string list) service
val closed_orders : int -> (form, Order.t assoc, string list) service
val trade_history : int -> (form, Filled_order.t assoc, string list) service
val ledgers : (form, Ledger.t assoc, string list) service
