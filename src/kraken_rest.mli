open Kraken
open Fastrest
open Json_encoding

val time : (form, Ptime.t) service

val assets : (form, Asset.t list) service
val symbols : (form, Pair.t list) service
val account_balance : (form, (string * float) list) service
val trade_balance : ?asset:string -> unit -> (form, Balance.t) service

val closed_orders :
  ?start:Ptime.t -> ?stop:Ptime.t -> ?ofs:int -> unit ->
  (form, Order.t list) service

val trade_history : int -> (form, Trade.t list) service

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

module Transfer : sig
  type status =
    | Success
    | Pending
    | Partial
    | Settled
    | Fail

  val string_of_status : status -> string

  type status_prop =
    | Return
    | OnHold

  type t = {
    meth: string;
    aclass: aclass;
    asset: string;
    refid: KrakID.t;
    txid: string;
    info: string;
    amount: float;
    fee: float;
    time: Ptime.t;
    status: status ;
    status_prop: status_prop option;
  } [@@deriving sexp_of]

  val pp : t Fmt.t
end

val transfer_status :
  asset:string -> meth:string ->
  [`Deposit|`Withdrawal] -> (form, Transfer.t list) service

type token = {
  token: string ;
  expires: Ptime.Span.t ;
}

val websocket_token : (form, token) service
