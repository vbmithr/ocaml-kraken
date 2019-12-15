open Core
open Kraken
open Fastrest
open Json_encoding

let base_url =
  Uri.make ~scheme:"https" ~host:"api.kraken.com" ()

let error =
  union [
    case (list string) (function [_] -> None | sl -> Some sl) (fun a -> a) ;
    case string (function [s] -> Some s | _ -> None) (fun a -> [a]) ;
  ]

let result_encoding encoding =
  conv
    (fun _ -> assert false)
    (function
      | (e, None) ->
        Error Error.(of_list (List.map ~f:of_string e))
      | (_, Some r) -> Ok r)
    (obj2
       (req "error" error)
       (opt "result" encoding))

let auth srv { key ; secret ; _ } =
  let ps = match srv.params with
    | Form ps -> ps
    | Json (_,_) -> assert false in
  let nonce = Time_ns.(to_int63_ns_since_epoch (now ())) in
  let ps = ("nonce", [Int63.to_string nonce]) :: ps in
  let encoded = Uri.encoded_of_query ps in
  let open Digestif in
  let digest =
    SHA256.(digest_string ((Int63.to_string nonce) ^ encoded) |>
            to_raw_string) in
  let sign =
    SHA512.(hmac_string ~key:secret (Uri.path srv.url ^ digest) |>
            to_raw_string) in
  let headers =
    Httpaf.Headers.of_list [
      "API-Key", key ;
      "API-Sign", Base64.encode_exn sign ;
    ] in
  { params = Form ps ; headers }

let time =
  let time_encoding =
    conv
      (fun t -> (), Int64.of_float (Ptime.to_float_s t))
      (fun ((), t) -> match Ptime.of_float_s (Int64.to_float t) with
         | None -> invalid_arg "time_encoding"
         | Some t -> t)
      (merge_objs unit (obj1 (req "unixtime" int53)))
  in
  get (result_encoding time_encoding)
    (Uri.with_path base_url "0/public/Time")


let boxed_list name encoding idx_of_string =
  conv
    (fun t -> t, 0l)
    (fun (t, _) -> t)
    (obj2
       (req name (kraklist encoding idx_of_string))
       (req "count" int32))

let trades = boxed_list "trades" Trade.encoding KrakID.of_string
let closed = boxed_list "closed" Order.encoding KrakID.of_string
let ledger = boxed_list "ledger" Ledger.encoding KrakID.of_string

let assets =
  get (result_encoding (kraklist Asset.encoding Fn.id))
    (Uri.with_path base_url "0/public/Assets")

let symbols =
  get (result_encoding (kraklist Pair.encoding Fn.id))
    (Uri.with_path base_url "0/public/AssetPairs")

let account_balance =
  let enc pair = conv (fun _ -> assert false) (fun a -> pair, a) strfloat in
  post_form ~auth (result_encoding (kraklist enc Fn.id))
    (Uri.with_path base_url "0/private/Balance")

let trade_balance ?(asset="ZUSD") () =
  let params = ["asset", [asset]] in
  post_form ~params ~auth (result_encoding Balance.encoding)
    (Uri.with_path base_url "0/private/TradeBalance")

let string_of_ptime t =
  Float.to_string (Ptime.to_float_s t)

let closed_orders ?start ?stop ?ofs () =
  let params = List.filter_opt [
      Option.map start ~f:(fun t -> "start", [string_of_ptime t]) ;
      Option.map stop ~f:(fun t -> "end", [string_of_ptime t]) ;
      Option.map ofs ~f:(fun ofs -> "ofs", [Int.to_string ofs]) ;
    ] in
  post_form ~auth ~params
    (result_encoding closed)
    (Uri.with_path base_url "0/private/ClosedOrders")

let trade_history ofs =
  post_form ~auth ~params:["ofs", [Int.to_string ofs]]
    (result_encoding trades)
    (Uri.with_path base_url "0/private/TradesHistory")

let ledgers ?assets ?typ ?start ?stop ?ofs () =
  let params = List.filter_opt [
      Option.map assets ~f:(fun assets -> "assets", assets) ;
      Option.map typ ~f:(fun typ -> "type", [Ledger.string_of_typ typ]) ;
      Option.map start ~f:(fun t -> "start", [string_of_ptime t]) ;
      Option.map stop ~f:(fun t -> "end", [string_of_ptime t]) ;
      Option.map ofs ~f:(fun ofs -> "ofs", [Int.to_string ofs]) ;
  ] in
  post_form ~params ~auth (result_encoding ledger)
    (Uri.with_path base_url "0/private/Ledgers")

type deposit_method = {
  meth: string;
  limit: float;
  fee: float;
  setup: float option;
  genAddr: bool option;
}

let limit =
  union [
    case strfloat (fun a -> Some a) (fun a -> a) ;
    case bool
      (fun a -> match Float.classify a with
         | Float.Class.Infinite -> Some false | _ -> None)
      (fun _ -> Float.infinity) ;
  ]

let deposit_method =
  conv
    (fun _ -> assert false)
    (fun (meth, limit, fee, setup, genAddr) -> {meth; limit; fee; setup; genAddr })
    (obj5
       (req "method" string)
       (req "limit" limit)
       (req "fee" strfloat)
       (opt "address-setup-fee" strfloat)
       (opt "gen-address" bool))

let deposit_methods ~asset =
  post_form ~auth ~params:["asset", [asset]]
    (result_encoding (list deposit_method))
    (Uri.with_path base_url "0/private/DepositMethods")

type addr = {
  addr: string;
  expireTm: Ptime.t;
  newAddr: bool option;
}

let addr =
  conv
    (fun _ -> assert false)
    (fun (addr, expireTm, newAddr) -> { addr; expireTm; newAddr})
    (obj3
       (req "address" string)
       (req "expiretm" Ptime.encoding)
       (opt "new" bool))

let deposit_addresses ~asset ~meth =
  post_form ~auth ~params:["asset", [asset]; "method", [meth]]
    (result_encoding (list addr))
    (Uri.with_path base_url "0/private/DepositAddresses")

module Transfer = struct
  type status =
    | Success
    | Pending
    | Partial
    | Settled
    | Fail
  [@@deriving sexp_of]

  let string_of_status = function
    | Success -> "Success"
    | Pending -> "Pending"
    | Partial -> "Partial"
    | Settled -> "Settled"
    | Fail -> "Failure"

  type status_prop =
    | Return
    | OnHold
  [@@deriving sexp_of]

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
    status: status;
    status_prop: status_prop option;
  } [@@deriving sexp_of]

  let pp ppf t =
    Format.fprintf ppf "%a" Sexplib.Sexp.pp (sexp_of_t t)

  let status_prop =
    string_enum [
      "return", Return;
      "onhold", OnHold;
    ]

  let status =
    string_enum [
      "Success", Success;
      "Failure", Fail;
      "Partial", Partial;
      "Settled", Settled;
      "Pending", Pending;
    ]

  let encoding =
    conv
      (fun _ -> assert false)
      (fun ((meth, aclass, asset, refid, txid,
             info, amount, fee, time, status), status_prop) ->
        { meth; aclass; asset; refid; txid; info;
          amount; fee; time; status; status_prop })
      (merge_objs
         (obj10
            (req "method" string)
            (req "aclass" aclass)
            (req "asset" string)
            (req "refid" KrakID.encoding)
            (req "txid" string)
            (req "info" string)
            (req "amount" strfloat)
            (req "fee" strfloat)
            (req "time" Ptime.encoding)
            (req "status" status))
         (obj1 (opt "status-prop" status_prop)))
end

let transfer_status ~asset ~meth kind =
  let path = match kind with
    | `Deposit -> "0/private/DepositStatus"
    | `Withdrawal -> "0/private/WithdrawStatus" in
  post_form ~auth ~params:["asset", [asset]; "method", [meth]]
    (result_encoding (list Transfer.encoding))
    (Uri.with_path base_url path)

type token = {
  token: string ;
  expires: Ptime.Span.t ;
}

let span = conv (fun _ -> assert false) Ptime.Span.of_int_s int

let token =
  conv (fun _ -> assert false)
    (fun (token, expires) -> { token; expires })
    (obj2
       (req "token" string)
       (req "expires" span))

let websocket_token =
  post_form ~auth
    (result_encoding token)
    (Uri.with_path base_url "0/private/GetWebSocketsToken")
