open Kraken
open Fastrest

val time : (get, Ptime.t, string list) service

type 'a assoc = (string * 'a) list [@@deriving sexp]

val asset_pairs : (get, (string * Pair.t) list, string list) service
val account_balance : (post_form, float assoc, string list) service
val trade_balance : (post_form, Balance.t, string list) service
val closed_orders : (post_form, Order.t assoc, string list) service
val trade_history : (post_form, Filled_order.t assoc, string list) service
val ledgers : (post_form, Ledger.t assoc, string list) service
