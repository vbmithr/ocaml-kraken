open Core
open Kraken
open Fastrest

let base_url =
  Uri.make ~scheme:"https" ~host:"api.kraken.com" ()

let result_encoding encoding =
  let open Json_encoding in
  conv
    (function Error e -> (e, None) | Ok v -> [], Some v)
    (function (e, None) -> Error e | (_, Some r) -> Ok r)
    (obj2
       (req "error" (list string))
       (opt "result" encoding))

let auth service { key ; secret ; _ } =
  let nonce = Time_ns.(to_int63_ns_since_epoch (now ())) in
  let params = ["nonce", [Int63.to_string nonce]] in
  let encoded = Uri.encoded_of_query (params @ service.params) in
  let open Digestif in
  let digest =
    SHA256.(digest_string ((Int63.to_string nonce) ^ encoded) |>
            to_raw_string) in
  let sign =
    SHA512.(hmac_string ~key:secret (Uri.path service.url ^ digest) |>
            to_raw_string) in
  let headers =
    Httpaf.Headers.of_list [
      "API-Key", key ;
      "API-Sign", Base64.encode_exn sign ;
    ] in
  { params ; headers }

(* type error = {
 *   severity : [`E | `W] ;
 *   cat : string ;
 *   msg : string ;
 *   extra : string option ;
 * } *)

let time =
  let time_encoding =
    let open Json_encoding in
    conv
      (fun t -> (), Int64.of_float (Ptime.to_float_s t))
      (fun ((), t) -> match Ptime.of_float_s (Int64.to_float t) with
         | None -> invalid_arg "time_encoding"
         | Some t -> t)
      (merge_objs unit (obj1 (req "unixtime" int53)))
  in
  get (result_encoding time_encoding)
    (Uri.with_path base_url "0/public/Time")


type 'a assoc = (string * 'a) list [@@deriving sexp]

let balances_encoding =
  let open Json_encoding in
  conv
    (fun s -> `O (List.map ~f:(fun (k, v) -> (k, `Float v)) s))
    (function
      | `O vs ->
        List.map ~f:(function
            | k, `String v -> k, float_of_string v
            | _ -> invalid_arg "balance_encoding") vs
      | #Ezjsonm.value -> invalid_arg "balance_encoding")
    any_ezjson_value

let list_encoding encoding =
  let open Json_encoding in
  conv
    (fun s -> `O (List.map ~f:(fun (k, v) ->
         (k, Json_encoding.construct encoding v)) s))
    (function
      | `O vs ->
        List.map ~f:begin fun (k, v) ->
          k, Ezjsonm_encoding.destruct_safe encoding v
        end vs
      | #Ezjsonm.value -> invalid_arg "list_encoding")
    any_ezjson_value

let boxed_list_encoding name encoding =
  let open Json_encoding in
  conv (fun t -> t, 0l) (fun (t, _) -> t)
    (obj2
       (req name (list_encoding encoding))
       (req "count" int32))

let trade_encoding = boxed_list_encoding "trades" Filled_order.encoding
let closed_encoding = boxed_list_encoding "closed" Order.encoding
let ledger_encoding = boxed_list_encoding "ledger" Ledger.encoding

let asset_pairs =
  get (result_encoding (list_encoding Pair.encoding))
    (Uri.with_path base_url "0/public/AssetPairs")

let account_balance =
  post_form ~auth (result_encoding balances_encoding)
    (Uri.with_path base_url "0/private/Balance")

let trade_balance =
  post_form ~auth (result_encoding Balance.encoding)
    (Uri.with_path base_url "0/private/TradeBalance")

let closed_orders =
  post_form ~auth (result_encoding closed_encoding)
    (Uri.with_path base_url "0/private/ClosedOrders")

let trade_history =
  post_form ~auth (result_encoding trade_encoding)
    (Uri.with_path base_url "0/private/TradesHistory")

let ledgers =
  post_form ~auth (result_encoding ledger_encoding)
    (Uri.with_path base_url "0/private/Ledgers")
