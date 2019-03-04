open Core
open Async
open Httpaf
open Kraken

type auth = {
  key : string ;
  secret : string ;
}

type get
type post

type _ meth =
  | Get : get meth
  | Post : post meth

type ('meth, 'a) service = {
  meth : 'meth meth ;
  url : Uri.t ;
  req : Request.t ;
  encoding : 'a Json_encoding.encoding ;
  pp : Format.formatter -> 'a -> unit ;
  params : (string * string list) list ;
}

let src =
  Logs.Src.create "kraken.rest"

let base_url =
  Uri.make ~scheme:"https" ~host:"api.kraken.com" ()

type error =
  | Http of Client_connection.error
  | Kraken of string list

let result_encoding encoding =
  let open Json_encoding in
  conv
    (function Error e -> (e, None) | Ok v -> [], Some v)
    (function (e, None) -> Error e | (_, Some r) -> Ok r)
    (obj2
       (req "error" (list string))
       (opt "result" encoding))

let get ?(params=[]) encoding pp url =
  let target = Uri.path_and_query url in
  let req = Request.create `GET target in
  { meth = Get ; url ; req ; encoding ; pp ; params }

let post ?(params=[]) encoding pp url =
  let target = Uri.path_and_query url in
  let req = Request.create `POST target in
  { meth = Post ; url ; req ; encoding ; pp ; params }

let authstr ~secret service =
  let nonce = Time_ns.(to_int63_ns_since_epoch (now ())) in
  let nonce_p = ("nonce", [Int63.to_string nonce]) in
  let encoded = Uri.encoded_of_query (nonce_p :: service.params) in
  let open Digestif in
  let digest =
    SHA256.(digest_string ((Int63.to_string nonce) ^ encoded) |>
            to_raw_string) in
  String.length encoded, nonce_p,
  SHA512.(hmac_string ~key:secret (Uri.path service.url ^ digest) |>
          to_raw_string)

let write_iovec w iovec =
  List.fold_left iovec ~init:0 ~f:begin fun a { Faraday.buffer ; off ; len } ->
    Writer.write_bigstring w buffer ~pos:off ~len ;
    a+len
  end

let request (type a) ?auth (service : (a, 'b) service) =
  let error_iv = Ivar.create () in
  let resp_iv = Ivar.create () in
  let error_handler err =
    Ivar.fill error_iv (Error (Http err))
  in
  let response_handler response body =
    Logs.debug ~src (fun m -> m "%a" Response.pp_hum response) ;
    match response with
    | { Response.status = `OK ; _ } ->
      let buffer = Buffer.create 32 in
      let on_eof () =
        let buf_str = Buffer.contents buffer in
        Logs.debug ~src (fun m -> m "%s" buf_str) ;
        let resp_json = Ezjsonm.from_string buf_str in
        match (Ezjsonm_encoding.destruct_safe
                 (result_encoding service.encoding) resp_json) with
        | Error e -> Ivar.fill error_iv (Error (Kraken e))
        | Ok v -> Ivar.fill resp_iv (Ok v)
      in
      let rec on_read buf ~off ~len =
        Buffer.add_string buffer (Bigstringaf.substring buf ~off ~len) ;
        Body.schedule_read body ~on_eof ~on_read
      in
      Body.schedule_read body ~on_eof ~on_read
    | _ ->
      Logs.err ~src (fun m -> m "Error response")
  in
  let params, headers = match service.meth, auth with
    | Get, _ -> service.params, service.req.headers
    | Post, None -> invalid_arg "post service needs auth"
    | Post, Some { key ; secret } ->
      let content_length, nonce_p, a = authstr ~secret service in
      nonce_p :: service.params,
      Headers.add_list service.req.headers [
        "API-Key", key ;
        "API-Sign", Base64.encode_exn a ;
        "Content-Type", "application/x-www-form-urlencoded" ;
        "Content-Length", string_of_int content_length ;
      ]
  in
  let headers = Headers.add_list headers [
      "User-Agent", "ocaml-kraken" ;
      "Host", Uri.host_with_default ~default:"api.kraken.com" service.url ;
    ] in
  let req = { service.req with headers } in
  Conduit_async.V3.with_connection_uri service.url begin fun _ r w ->
    let body, conn =
      Client_connection.request req ~error_handler ~response_handler in
    let rec flush_req () =
      match Client_connection.next_write_operation conn with
      | `Write iovec ->
        let nb_read = write_iovec w iovec in
        Client_connection.report_write_result conn (`Ok nb_read) ;
        flush_req ()
      | `Yield ->
        Client_connection.yield_writer conn flush_req ;
      | `Close _ -> () in
    let rec read_response () =
      match Client_connection.next_read_operation conn with
      | `Close -> Deferred.unit
      | `Read -> begin
          Reader.read_one_chunk_at_a_time r
            ~handle_chunk:begin fun buf ~pos ~len ->
              let nb_read = Client_connection.read conn buf ~off:pos ~len in
              return (`Stop_consumed ((), nb_read))
            end >>= function
          | `Eof -> Deferred.unit
          | `Eof_with_unconsumed_data _ -> Deferred.unit
          | `Stopped () -> read_response ()
        end in
    Logs_async.debug ~src
      (fun m -> m "%a" Request.pp_hum req) >>= fun () ->
    begin
      match service.meth, params with
      | Post, params ->
        let encoded_params = Uri.encoded_of_query params in
        Logs.debug ~src (fun m -> m "%s" encoded_params) ;
        Body.write_string body encoded_params
      | _ -> ()
    end ;
    flush_req () ;
    don't_wait_for (read_response ()) ;
    Deferred.any [Ivar.read resp_iv ;
                  Ivar.read error_iv]
  end

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
  get time_encoding Ptime.pp (Uri.with_path base_url "0/public/Time")


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

let pp_balance ppf t =
  Format.fprintf ppf "%a" Sexp.pp (sexp_of_assoc sexp_of_float t)

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

let pp_trade ppf t =
  Format.fprintf ppf "%a" Sexp.pp (sexp_of_assoc Filled_order.sexp_of_t t)

let pp_closed ppf t =
  Format.fprintf ppf "%a" Sexp.pp (sexp_of_assoc Order.sexp_of_t t)

let pp_ledger ppf t =
  Format.fprintf ppf "%a" Sexp.pp (sexp_of_assoc Ledger.sexp_of_t t)

let account_balance =
  post balances_encoding pp_balance
    (Uri.with_path base_url "0/private/Balance")

open Kraken

let trade_balance =
  post Balance.encoding Balance.pp
    (Uri.with_path base_url "0/private/TradeBalance")

let closed_orders =
  post closed_encoding pp_closed
    (Uri.with_path base_url "0/private/ClosedOrders")

let trade_history =
  post trade_encoding pp_trade
    (Uri.with_path base_url "0/private/TradesHistory")

let ledgers =
  post ledger_encoding pp_ledger
    (Uri.with_path base_url "0/private/Ledgers")
