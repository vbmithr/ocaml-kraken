open Kraken
open Fastrest
open Json_encoding

val time : (form, Ptime.t) service

val asset_pairs : (form, Pair.t list) service
val account_balance : (form, (string * float) list) service
val trade_balance : (form, Balance.t) service
val closed_orders : int -> (form, Order.t list) service
val trade_history : int -> (form, Filled_order.t list) service

val ledgers :
  ?assets:string list ->
  ?typ:Ledger.typ ->
  ?start:Ptime.t ->
  ?stop:Ptime.t ->
  ?ofs:int -> unit -> (form, Ledger.t list) service

type deposit_method = {
  meth: string;
  limit: float;
  fee: float;
  setup: float option;
  genAddr: bool option;
}
val deposit_method : deposit_method encoding
val deposit_methods : asset:string -> (form, deposit_method list) service

type addr = {
  addr: string;
  expireTm: Ptime.t;
  newAddr: bool option;
}

val deposit_addresses : asset:string -> meth:string -> (form, addr list) service

type deposit = {
  meth: string;
  aclass: aclass;
  asset: string;
  refid: string;
  txid: string;
  info: string;
  amount: float;
  fee: float;
  time: Ptime.t;
  status: [`Success|`Failure|`Partial];
  status_prop: [`Return|`OnHold] option;
}

val deposit_status : asset:string -> meth:string -> (form, deposit list) service
val withdraw_status : asset:string -> meth:string -> (form, string list) service
