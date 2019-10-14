open Sexplib.Std
open Kraken

module Pair = struct
  type t = {
    base: string ;
    quote: string ;
  } [@@deriving sexp]

  let compare { base ; quote } { base = base' ; quote = quote' } =
    match String.compare base base' with
    | 0 -> String.compare quote quote'
    | n -> n

  let pp ppf { base ; quote } =
    Format.fprintf ppf "%s/%s" base quote

  let to_string { base ; quote } =
    base ^ "/" ^ quote

  let of_string s =
    match String.split_on_char '/' s with
    | [base ; quote] -> Some { base ; quote }
    | _ -> None

  let of_string_exn s =
    match String.split_on_char '/' s with
    | [base ; quote] -> { base ; quote }
    | _ -> invalid_arg "pair_of_string_exn"

  let encoding =
    Json_encoding.(conv to_string of_string_exn string)
end

type subscription =
  | Ticker
  | OHLC of int
  | Trade
  | Book of int
  | Spread
  | All
[@@deriving sexp]

let subscription_encoding =
  let open Json_encoding in
  conv
    (function
      | Ticker -> "ticker", None, None
      | OHLC i -> "ohlc", Some i, None
      | Trade -> "trade", None, None
      | Book i -> "book", None, Some i
      | Spread -> "spread", None, None
      | All -> "*", None, None
    )
    (function
      | "ticker", _, _ -> Ticker
      | "ohlc", None, _ -> OHLC 1
      | "ohlc", Some i, _ -> OHLC i
      | "trade", _, _ -> Trade
      | "book", _, None -> Book 10
      | "book", _, Some i -> Book i
      | "spread", _, _ -> Spread
      | "*", _, _ -> All
      | _ -> invalid_arg "subscription_encoding"
    )
    (obj3
       (req "name" string)
       (opt "interval" int)
       (opt "depth" int))

type subscriptionStatus =
  | Subscribed
  | Unsubscribed
  | Error of string
[@@deriving sexp]

let subscriptionStatus_encoding =
  let open Json_encoding in
  string_enum [
    "subscribed", Subscribed ;
    "unsubscribed", Unsubscribed ;
    "error", Error "" ;
  ]

type status = {
  connectionID: float ;
  status: string ;
  version: string
} [@@deriving sexp]

let status_encoding =
  let open Json_encoding in
  conv
    (fun { connectionID ; status ; version } -> ((), connectionID, status, version))
    (fun ((), connectionID, status, version) -> { connectionID ; status ; version })
    (obj4
       (req "event" (constant "systemStatus"))
       (req "connectionID" float)
       (req "status" string)
       (req "version" string))

type subscribe = {
  reqid: int option ;
  pairs: Pair.t list ;
  sub: subscription ;
} [@@deriving sexp]

let tickers ?reqid pairs = { reqid ; pairs ; sub = Ticker }
let trades ?reqid pairs = { reqid ; pairs ; sub = Trade }
let book10 ?reqid pairs = { reqid ; pairs ; sub = Book 10 }
let book25 ?reqid pairs = { reqid ; pairs ; sub = Book 25 }
let book100 ?reqid pairs = { reqid ; pairs ; sub = Book 100 }
let book500 ?reqid pairs = { reqid ; pairs ; sub = Book 500 }
let book1000 ?reqid pairs = { reqid ; pairs ; sub = Book 1000 }

let subscribe_encoding =
  let open Json_encoding in
  conv
    (fun { reqid ; pairs ; sub } ->
       ((), reqid, List.map Pair.to_string pairs, sub))
    (fun ((), reqid, pair, sub) ->
       let pairs = List.map Pair.of_string_exn pair in
       { reqid ; pairs ; sub })
    (obj4
     (req "event" (constant "subscribe"))
     (opt "reqid" int)
     (req "pair" (list string))
     (req "subscription" subscription_encoding))

type subscription_status = {
  chanid : int ;
  name : string ;
  pair : Pair.t ;
  status : subscriptionStatus ;
  reqid : int option ;
  subscription : subscription ;
} [@@deriving sexp]

let subscription_status_encoding =
  let open Json_encoding in
  conv
    (fun { chanid ; name ; pair ; status ; reqid ; subscription } ->
       ((), reqid, status, chanid, name, pair, subscription))
    (fun ((), reqid, status, chanid, name, pair, subscription) ->
       { reqid ; status ; chanid ; name ; pair ; subscription })
  (obj7
     (req "event" (constant "subscriptionStatus"))
     (opt "reqid" int)
     (req "status" subscriptionStatus_encoding)
     (req "channelID" int)
     (req "channelName" string)
     (req "pair" Pair.encoding)
     (req "subscription" subscription_encoding))

type error = {
  reqid : int option ;
  msg : string
} [@@deriving sexp]

let error_encoding =
  let open Json_encoding in
  conv
    (fun { reqid ; msg } -> (), ((), reqid, (), msg))
    (fun ((), ((), reqid, (), msg)) -> { reqid ; msg })
    (merge_objs unit
    ((obj4
        (req "event" (constant "subscriptionStatus"))
        (opt "reqid" int)
        (req "status" (constant "error"))
        (req "errorMessage" string))))

type unsubscribe = {
  chanid : int ;
  reqid : int option
} [@@deriving sexp]

let unsubscribe_encoding =
  let open Json_encoding in
  conv
    (fun { chanid ; reqid } -> ((), reqid, chanid))
    (fun ((), reqid, chanid) -> { chanid ; reqid })
    (obj3
       (req "event" (constant "unsubscribe"))
       (opt "reqid" int)
       (req "channelID" int))

type a = {
  price: float ;
  wholeLotVolume : int ;
  lotVolume : float
} [@@deriving sexp]

let a_encoding =
  let open Json_encoding in
  conv
    (fun { price; wholeLotVolume; lotVolume} -> ( price, wholeLotVolume, lotVolume ))
    (fun ( price, wholeLotVolume, lotVolume) -> { price; wholeLotVolume; lotVolume })
    (tup3 strfloat int strfloat)

type b = {
  price: float ;
  wholeLotVolume : int ;
  lotVolume : float
} [@@deriving sexp]

let b_encoding =
  let open Json_encoding in
  conv
    (fun { price; wholeLotVolume; lotVolume} -> ( price, wholeLotVolume, lotVolume ))
    (fun ( price, wholeLotVolume, lotVolume) -> { price; wholeLotVolume; lotVolume })
    (tup3 strfloat int strfloat)

type c = {
  price: float ;
  lotVolume : float
} [@@deriving sexp]

let c_encoding =
  let open Json_encoding in
  conv
    (fun { price; lotVolume} -> ( price, lotVolume ))
    (fun ( price, lotVolume) -> { price; lotVolume })
    (tup2 strfloat strfloat)

type v = {
  today: float ;
  last24Hours: float
} [@@deriving sexp]

let v_encoding =
  let open Json_encoding in
  conv
    (fun { today; last24Hours } -> ( today, last24Hours ))
    (fun ( today, last24Hours ) -> { today; last24Hours })
    (tup2 strfloat strfloat)

type p = {
  today: float ;
  last24Hours: float
} [@@deriving sexp]

let p_encoding =
  let open Json_encoding in
  conv
    (fun { today; last24Hours } -> ( today, last24Hours ))
    (fun ( today, last24Hours ) -> { today; last24Hours })
    (tup2 strfloat strfloat)

type ti = {
  today: int ;
  last24Hours: int
} [@@deriving sexp]

let ti_encoding =
  let open Json_encoding in
  conv
    (fun { today; last24Hours } -> ( today, last24Hours ))
    (fun ( today, last24Hours ) -> { today; last24Hours })
    (tup2 int int)

type l = {
  today: float ;
  last24Hours: float
} [@@deriving sexp]

let l_encoding =
  let open Json_encoding in
  conv
    (fun { today; last24Hours } -> ( today, last24Hours ))
    (fun ( today, last24Hours ) -> { today; last24Hours })
    (tup2 strfloat strfloat)

type h = {
  today: float ;
  last24Hours: float
} [@@deriving sexp]

let h_encoding =
  let open Json_encoding in
  conv
    (fun { today; last24Hours } -> ( today, last24Hours ))
    (fun ( today, last24Hours ) -> { today; last24Hours })
    (tup2 strfloat strfloat)

type o = {
  today: float ;
  last24Hours: float
} [@@deriving sexp]

let o_encoding =
  let open Json_encoding in
  conv
    (fun { today; last24Hours } -> ( today, last24Hours ))
    (fun ( today, last24Hours ) -> { today; last24Hours })
    (tup2 strfloat strfloat)

type ticker = {
  a: a;
  b: b;
  c: c;
  v: v;
  p: p;
  t: ti;
  l: l;
  h: h;
  o: o;
} [@@deriving sexp]

let ticker_encoding =
  let open Json_encoding in
  conv
    (fun { a; b; c; v; p; t; l; h; o } ->
       (a, b, c, v, p, t, l, h, o ))
    (fun ( a, b, c, v, p, t, l, h, o ) ->
       { a; b; c; v; p; t; l; h; o })
    (obj9
       (req "a" (a_encoding))
       (req "b" (b_encoding))
       (req "c" (c_encoding))
       (req "v" (v_encoding))
       (req "p" (p_encoding))
       (req "t" (ti_encoding))
       (req "l" (l_encoding))
       (req "h" (h_encoding))
       (req "o" (o_encoding)))

type trade = {
  price: float ;
  qty: float ;
  ts: Ptime.t ;
  side: Fixtypes.Side.t ;
  ord_type: Fixtypes.OrdType.t ;
  misc: string ;
} [@@deriving sexp]

let side_encoding =
  let open Json_encoding in
  string_enum [
    "b", Fixtypes.Side.Buy ;
    "s", Sell ;
  ]

let ord_type_encoding =
  let open Json_encoding in
  string_enum [
    "l", Fixtypes.OrdType.Limit ;
    "m", Market ;
  ]

let trade_encoding =
  let open Json_encoding in
  conv
    (fun { price ; qty ; ts ; side ; ord_type ; misc } ->
       (price, qty, ts, side, ord_type, misc))
    (fun (price, qty, ts, side, ord_type, misc) ->
       { price ; qty ; ts ; side ; ord_type ; misc })
    (tup6 strfloat strfloat Ptime.encoding
       side_encoding ord_type_encoding string)

type quote = {
  price: float ;
  qty: float ;
  ts: Ptime.t ;
  republish: bool ;
} [@@deriving sexp]

let book_entry_encoding =
  let open Json_encoding in
  union [
    case (tup3 strfloat strfloat Ptime.encoding)
      (fun { price ; qty ; ts ; _ } -> Some (price, qty, ts))
      (fun (price, qty, ts) -> { price ; qty ; ts ; republish = false }) ;
    case (tup4 strfloat strfloat Ptime.encoding (constant "r"))
      (function { price ; qty ; ts ; republish } ->
         if republish then Some (price, qty, ts, ()) else None)
      (fun (price, qty, ts, ()) -> { price ; qty ; ts ; republish = true }) ;
  ]

type book = {
  asks : quote list ;
  bids : quote list ;
} [@@deriving sexp]

let snap_encoding =
  let open Json_encoding in
  conv
    (fun { asks ; bids } -> (asks, bids))
    (fun (asks, bids) -> { asks ; bids })
    (obj2
       (req "bs" (list book_entry_encoding))
       (req "as" (list book_entry_encoding)))

let book_encoding =
  let open Json_encoding in
  conv
    (fun { asks ; bids } -> (asks, bids))
    (fun (asks, bids) -> { asks ; bids })
    (obj2
       (dft "a" (list book_entry_encoding) [])
       (dft "b" (list book_entry_encoding) []))

type t =
  | Ping of int option
  | Pong of int option
  | HeartBt
  | Status of status
  | Subscribe of subscribe
  | Unsubscribe of unsubscribe
  | Error of error
  | SubscriptionStatus of subscription_status
  | Ticker of ticker update
  | Trade of trade list update
  | Snapshot of book update
  | Quotes of book update

and 'a update = {
  chanid: int ;
  feed: string ;
  pair: Pair.t ;
  data: 'a ;
}
[@@deriving sexp]

let full_book_update_encoding =
  let open Json_encoding in
  conv
    (fun { chanid ; feed ; pair ; data = { asks ; bids } } ->
       (chanid, { asks ; bids = [] }, { asks = [] ; bids }, feed, pair))
    (fun (chanid, { asks ; _ }, { bids ; _ }, feed, pair) ->
       { chanid ; feed ; pair ; data = { asks ; bids } })
    (tup5 int book_encoding book_encoding string Pair.encoding)

let update_encoding enc =
  let open Json_encoding in
  conv
    (fun { chanid; feed; pair; data } -> (chanid, data, feed, pair))
    (fun (chanid, data, feed, pair) -> { chanid; feed; pair; data })
    (tup4 int enc string Pair.encoding)

let pp ppf t =
  Format.fprintf ppf "%a" Sexplib.Sexp.pp_hum (sexp_of_t t)

let ping_encoding =
  let open Json_encoding in
  conv
    (fun reqid -> ((), reqid))
    (fun ((), reqid) -> reqid)
    (obj2
       (req "event" (constant "ping"))
       (opt "reqid" int))

let pong_encoding =
  let open Json_encoding in
  conv
    (fun reqid -> ((), reqid))
    (fun ((), reqid) -> reqid)
    (obj2
       (req "event" (constant "pong"))
       (opt "reqid" int))

let hb_encoding =
  let open Json_encoding in
  obj1
    (req "event" (constant "heartbeat"))

let encoding =
  let open Json_encoding in
  union [
    case ping_encoding (function Ping reqid -> Some reqid | _ -> None) (fun reqid -> Ping reqid) ;
    case pong_encoding (function Pong reqid -> Some reqid | _ -> None) (fun reqid -> Pong reqid) ;
    case hb_encoding (function HeartBt -> Some () | _ -> None) (fun () -> HeartBt) ;
    case status_encoding (function Status s -> Some s | _ -> None) (fun s -> Status s) ;
    case error_encoding (function Error e -> Some e | _ -> None) (fun e -> Error e) ;
    case subscription_status_encoding (function SubscriptionStatus s -> Some s | _ -> None) (fun s -> SubscriptionStatus s) ;
    case subscribe_encoding (function Subscribe v -> Some v | _ -> None) (fun v -> Subscribe v) ;
    case unsubscribe_encoding (function Unsubscribe v -> Some v | _ -> None) (fun v -> Unsubscribe v) ;
    case (update_encoding ticker_encoding) (function Ticker t -> Some t | _ -> None) (fun t -> Ticker t) ;
    case (update_encoding (list trade_encoding)) (function Trade t -> Some t | _ -> None) (fun t -> Trade t) ;
    case (update_encoding snap_encoding) (function Snapshot s -> Some s | _ -> None) (fun s -> Snapshot s) ;
    case (update_encoding book_encoding) (function Quotes b -> Some b | _ -> None) (fun b -> Quotes b) ;
    case full_book_update_encoding (function Quotes b -> Some b | _ -> None) (fun b -> Quotes b) ;
  ]
