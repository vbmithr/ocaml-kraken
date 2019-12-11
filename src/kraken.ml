open Sexplib.Std
open Json_encoding

module KrakID = struct
  let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

  let idx = String.index alphabet
  let chr idx = alphabet.[idx]

  type kind =
    | Order
    | Deposit
    | Withdrawal
    | Trade
    | Ledger

  type t = int array
  let zero = [|0;0;0;0|]

  let get32 s p = Int32.to_int (EndianString.BigEndian.get_int32 s p)

  let of_guid u =
    let s = Uuidm.to_bytes u in
    let a = get32 s 0 in
    let b = get32 s 4 in
    let c = get32 s 8 in
    let d = get32 s 12 in
    [|a;b;c;d|]

  let to_guid t =
    let buf = Bytes.create 16 in
    Array.iteri (fun i v -> EndianBytes.BigEndian.set_int32 buf (i*4) (Int32.of_int v)) t ;
    let buf = Bytes.unsafe_to_string buf in
    Option.get (Uuidm.of_bytes buf)

  let get_firsts t =
    Array.map (fun v -> try Some (chr (v lsr 24 - 1)) with _ -> None) t

  let length t =
    if t.(3) lsr 24 > 0 then 20
    else if t.(2) lsr 24 > 0 then 19
    else if t.(1) lsr 24 > 0 then 18
    else 17

  let kind_of_char = function
    | 'O' -> Order
    | 'Q' | 'R' -> Deposit
    | 'A' | 'B' -> Withdrawal
    | 'T' -> Trade
    | 'L' -> Ledger
    | c -> invalid_arg (Printf.sprintf "kind_of_char: %c" c)

  let kind t = kind_of_char (chr (t.(0) lsr 24))

  let int_of_4 s pos =
    let rec inner mult sum i =
      if i >= 0 then
        inner (mult*36) (sum+mult*idx s.[pos+i]) (pred i)
      else sum
    in inner 1 0 3

  let chars_of_int buf pos x =
    let rec inner a i =
      if i >= 0 then begin
        Bytes.set buf (pos+i) (chr (a mod 36)) ;
        inner (a / 36) (pred i)
      end in
    inner (x land 0xffffff) 3

  let of_string s =
    match String.split_on_char '-' s with
    | [a;b;c] ->
      let buf = a ^ b ^ c in
      let _ = kind_of_char a.[0] in
      let offset = String.length buf - 16 in
      let res = Array.init 4 (fun i -> int_of_4 buf (offset+i*4)) in
      for i = 0 to offset - 1 do
        res.(i) <- res.(i) lor ((1 + idx buf.[i]) lsl 24) ;
      done ;
      res
    | _ -> invalid_arg "KrakID.of_string"

  let to_chars t =
    let len = length t in
    let res = Bytes.create len in
    let firsts = get_firsts t in
    let offset = len - 16 in
    Array.iteri (fun i v -> Option.iter (Bytes.set res i) v) firsts ;
    Array.iteri (fun i v -> chars_of_int res (offset+i*4) v) t ;
    Bytes.unsafe_to_string res

  let pp ppf t =
    let buf = to_chars t in
    match length t with
    | 17 -> Format.fprintf ppf "%c%c%c%c%c%c-%c%c%c%c%c-%c%c%c%c%c%c"
              buf.[0] buf.[1] buf.[2] buf.[3] buf.[4] buf.[5]
              buf.[6] buf.[7] buf.[8] buf.[9] buf.[10]
              buf.[11] buf.[12] buf.[13] buf.[14] buf.[15] buf.[16]
    | 18 -> Format.fprintf ppf "%c%c%c%c%c%c%c-%c%c%c%c%c-%c%c%c%c%c%c"
              buf.[0] buf.[1] buf.[2] buf.[3] buf.[4] buf.[5]
              buf.[6] buf.[7] buf.[8] buf.[9] buf.[10]
              buf.[11] buf.[12] buf.[13] buf.[14] buf.[15] buf.[16] buf.[17]
    | 19 -> Format.fprintf ppf "%c%c%c%c%c%c%c-%c%c%c%c%c%c-%c%c%c%c%c%c"
              buf.[0] buf.[1] buf.[2] buf.[3] buf.[4] buf.[5]
              buf.[6] buf.[7] buf.[8] buf.[9] buf.[10]
              buf.[11] buf.[12] buf.[13] buf.[14] buf.[15] buf.[16] buf.[17] buf.[18]
    | n -> invalid_arg ("pp: " ^ string_of_int n)

  let to_string t =
    Format.asprintf "%a" pp t

  let sexp_of_t t = sexp_of_string (to_string t)

  let encoding =
    conv to_string of_string string
end

let strfloat =
  union [
    case float (fun s -> Some s) (fun s -> s) ;
    case string (fun s -> Some (string_of_float s)) float_of_string ;
  ]

let side_encoding =
  string_enum [
    "buy", Fixtypes.Side.Buy ;
    "sell", Sell ;
  ]

module Ezjsonm_encoding = struct
  include Json_encoding.Make(Json_repr.Ezjsonm)

  let destruct_safe encoding value =
    try destruct encoding value with exn ->
      Format.eprintf "%a@."
        (Json_encoding.print_error ?print_unknown:None) exn ;
      raise exn
end

module Ptime = struct
  include Ptime

  type date_ = int * int * int [@@deriving sexp]

  let date_of_sexp = date__of_sexp
  let sexp_of_date = sexp_of_date_

  let t_of_sexp sexp =
    let sexp_str = string_of_sexp sexp in
    match of_rfc3339 sexp_str with
    | Ok (t, _, _) -> t
    | _ -> invalid_arg "Ptime.t_of_sexp"

  let sexp_of_t t =
    sexp_of_string (to_rfc3339 t)

  let encoding =
    conv
      Ptime.to_float_s
      (fun ts -> match Ptime.of_float_s ts with
         | None -> invalid_arg "Ptime.encoding"
         | Some ts -> ts)
      strfloat
end

module OrdType = struct
  type t = Fixtypes.OrdType.t [@@deriving sexp]

  let encoding =
    string_enum [
      "market", Fixtypes.OrdType.Market ;
      "limit", Limit ;
      "stop-loss", Stop ;
      "stop-loss-limit", StopLimit ;
      "take-profit", MarketIfTouched ;
    ]
end

module OrdStatus = struct
  type t = Fixtypes.OrdStatus.t [@@deriving sexp]

  let encoding =
    string_enum [
      "pending", Fixtypes.OrdStatus.PendingNew ;
      "open", New ;
      "closed", Filled ;
      "canceled", Canceled ;
      "expired", Canceled ;
    ]
end

module Balance = struct
  type t = {
    equivalent_balance : float ;
    trade_balance : float ;
    total_margin : float ;
    pnl : float ;
    positions_cost : float ;
    positions_value : float ;
    equity : float ;
    free_margin : float ;
  } [@@deriving sexp]

  let pp ppf t =
    Format.fprintf ppf "%a" Sexplib.Sexp.pp (sexp_of_t t)

  let encoding =
    conv
      (fun { equivalent_balance ;
             trade_balance ; total_margin ;
             pnl ; positions_cost ; positions_value ;
             equity ; free_margin } ->
        (equivalent_balance, trade_balance,
         total_margin, pnl, positions_cost, positions_value,
         equity, free_margin))
      (fun (equivalent_balance, trade_balance,
            total_margin, pnl, positions_cost, positions_value,
            equity, free_margin) ->
        { equivalent_balance ;
          trade_balance ; total_margin ;
          pnl ; positions_cost ; positions_value ;
          equity ; free_margin })
      (obj8
         (req "eb" strfloat)
         (req "tb" strfloat)
         (req "m" strfloat)
         (req "n" strfloat)
         (req "c" strfloat)
         (req "v" strfloat)
         (req "e" strfloat)
         (req "mf" strfloat))
end

module Order = struct
  type descr = {
    pair: string ;
    side: Fixtypes.Side.t ;
    ord_type: OrdType.t ;
    price: float ;
    price2: float ;
    leverage: string ;
    order: string ;
    close: string option ;
  } [@@deriving sexp]

  let descr_encoding =
    conv
      (fun { pair ; side ; ord_type ; price ;
             price2 ; leverage ; order ; close } ->
        (pair, side, ord_type, price,
         price2, leverage, order, close))
      (fun (pair, side, ord_type, price,
            price2, leverage, order, close) ->
        { pair ; side ; ord_type ; price ;
          price2 ; leverage ; order ; close })
      (obj8
         (req "pair" string)
         (req "type" side_encoding)
         (req "ordertype" OrdType.encoding)
         (req "price" strfloat)
         (req "price2" strfloat)
         (req "leverage" string)
         (req "order" string)
         (opt "close" string))

  type t = {
    id: KrakID.t ;
    status: OrdStatus.t ;
    opentm: Ptime.t option ;
    closetm: Ptime.t option ;
    starttm: Ptime.t option ;
    expiretm: Ptime.t option ;
    descr: descr;
    vol: float ;
    vol_exec: float ;
    cost: float ;
    fee: float ;
    price: float ;
    stopprice: float option ;
    limitprice: float option ;
    misc: string ;
    oflags: string ;
  } [@@deriving sexp_of]

  let pp ppf t =
    Format.fprintf ppf "%a" Sexplib.Sexp.pp (sexp_of_t t)

  let otherfields_encoding =
    obj4
      (req "descr" descr_encoding)
      (req "status" OrdStatus.encoding)
      (req "misc" string)
      (req "oflags" string)

  let times_encoding =
    obj4
      (opt "opentm" Ptime.encoding)
      (opt "closetm" Ptime.encoding)
      (opt "starttm" Ptime.encoding)
      (opt "expiretm" Ptime.encoding)

  let floats_encoding =
    obj7
      (req "vol" strfloat)
      (req "vol_exec" strfloat)
      (req "cost" strfloat)
      (req "fee" strfloat)
      (req "price" strfloat)
      (opt "stopprice" strfloat)
      (opt "limitprice" strfloat)

  let encoding id =
    conv
      (fun _ -> assert false)
      (fun ((((), (descr, status, misc, oflags)),
             (opentm, closetm, starttm, expiretm)),
            (vol, vol_exec, cost, fee, price, stopprice, limitprice)) ->
        { id; status ; opentm ; closetm ; starttm ; expiretm ;
          descr ; vol ; vol_exec ; cost ; fee ; price ;
          stopprice ; limitprice ; misc ; oflags
        })
      (merge_objs
         (merge_objs
            (merge_objs unit otherfields_encoding)
            times_encoding)
         floats_encoding)
end

module Filled_order = struct
  type t = {
    id: KrakID.t ;
    ordertxid: KrakID.t ;
    postxid: KrakID.t option ;
    pair: string ;
    time: Ptime.t ;
    side: Fixtypes.Side.t ;
    ord_type: OrdType.t ;
    price: float ;
    cost: float ;
    fee: float ;
    vol: float ;
    margin: float ;
    misc: string ;
  } [@@deriving sexp_of]

  let pp ppf t =
    Format.fprintf ppf "%a" Sexplib.Sexp.pp (sexp_of_t t)

  let encoding id =
    conv
      (fun _ -> assert false)
      (fun ((ordertxid, pair, time, side, ord_type, price, cost, fee, vol, margin), (misc, postxid)) ->
         { id; ordertxid ; postxid ; pair ; time ; side ; ord_type ;
           price ; cost ; fee ; vol ; margin ; misc })
      (merge_objs
         (obj10
            (req "ordertxid" KrakID.encoding)
            (req "pair" string)
            (req "time" Ptime.encoding)
            (req "type" side_encoding)
            (req "ordertype" OrdType.encoding)
            (req "price" strfloat)
            (req "cost" strfloat)
            (req "fee" strfloat)
            (req "vol" strfloat)
            (req "margin" strfloat))
         (obj2
            (req "misc" string)
            (opt "postxid" KrakID.encoding)))
end

type aclass = [`currency] [@@deriving sexp]

let aclass =
  string_enum ["currency", `currency]

module Ledger = struct
  type typ =
    | Deposit
    | Withdrawal
    | Trade
    | Margin
    | Transfer [@@deriving sexp]

  let string_of_typ = function
    | Deposit -> "deposit"
    | Withdrawal -> "withdrawal"
    | Trade -> "trade"
    | Margin -> "margin"
    | Transfer -> "transfer"

  let typ_of_string = function
    | "deposit" -> Deposit
    | "withdrawal"-> Withdrawal
    | "trade"-> Trade
    | "margin"-> Margin
    | "transfer"-> Transfer
    | _ -> invalid_arg "Ledger.typ_of_string"

  let typ = string_enum [
      "deposit", Deposit ;
      "withdrawal", Withdrawal ;
      "trade", Trade ;
      "margin", Margin ;
      "transfer", Transfer ;
    ]

  type t = {
    id: KrakID.t ;
    refid : KrakID.t ;
    time : Ptime.t ;
    typ : typ ;
    aclass : aclass ;
    asset : string ;
    amount : float ;
    fee : float ;
    balance : float ;
  } [@@deriving sexp_of]

  let pp ppf t =
    Format.fprintf ppf "%a" Sexplib.Sexp.pp (sexp_of_t t)

  let encoding id =
    conv (fun _ -> assert false)
      (fun (refid, time, typ, aclass, asset, amount, fee, balance) ->
         { id; refid ; time ; typ ; aclass ; asset ; amount ; fee ; balance })
      (obj8
         (req "refid" KrakID.encoding)
         (req "time" Ptime.encoding)
         (req "type" typ)
         (req "aclass" aclass)
         (req "asset" string)
         (req "amount" strfloat)
         (req "fee" strfloat)
         (req "balance" strfloat))
end

module Pair = struct
  type t = {
    name: string ;
    altname: string ;
    wsname: string option ;
    aclass_base: aclass ;
    base: string ;
    aclass_quote: aclass ;
    quote: string ;
    pair_decimals: int ;
  } [@@deriving sexp]

  let pp ppf t =
    Format.fprintf ppf "%a" Sexplib.Sexp.pp (sexp_of_t t)

  let encoding name =
    conv
      (fun _ -> assert false)
      (fun ((), (altname, wsname, aclass_base, base, aclass_quote, quote, pair_decimals)) ->
         { name; altname ; wsname ; aclass_base ; base ; aclass_quote ; quote ; pair_decimals })
      (merge_objs unit
         (obj7
            (req "altname" string)
            (opt "wsname" string)
            (req "aclass_base" aclass)
            (req "base" string)
            (req "aclass_quote" aclass)
            (req "quote" string)
            (req "pair_decimals" int)))
end
