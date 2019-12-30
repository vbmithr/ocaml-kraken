open Sexplib.Std
open Json_encoding
open Kraken

let url_public = Uri.make ~scheme:"https" ~host:"ws.kraken.com" ()
let url_auth = Uri.make ~scheme:"https" ~host:"ws-auth.kraken.com" ()
let url_beta = Uri.make ~scheme:"https" ~host:"ws-beta.kraken.com" ()

type subscription =
  | Ticker
  | OHLC of int
  | Trade
  | Book of int
  | Spread
  | All

  | OwnTrades of string
  | OpenOrders of string
[@@deriving sexp_of]

let subscription =
  conv
    (function
      | Ticker -> "ticker", None, None, None
      | OHLC i -> "ohlc", Some i, None, None
      | Trade -> "trade", None, None, None
      | Book i -> "book", None, Some i, None
      | Spread -> "spread", None, None, None
      | All -> "*", None, None, None
      | OwnTrades token -> "ownTrades", None, None, Some token
      | OpenOrders token -> "openOrders", None, None, Some token
    )
    (function
      | "ticker", _, _, _ -> Ticker
      | "ohlc", None, _, _ -> OHLC 1
      | "ohlc", Some i, _, _ -> OHLC i
      | "trade", _, _, _-> Trade
      | "book", _, None, _ -> Book 10
      | "book", _, Some i, _ -> Book i
      | "spread", _, _, _ -> Spread
      | "*", _, _, _ -> All
      | "ownTrades", _, _, _ -> OwnTrades ""
      | "openOrders", _, _, _ -> OpenOrders ""
      | _ -> invalid_arg "subscription_encoding"
    )
    (obj4
       (req "name" string)
       (opt "interval" int)
       (opt "depth" int)
       (opt "token" string))

type status = {
  connectionID: float ;
  status: string ;
  version: string
} [@@deriving sexp_of]

let status =
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
} [@@deriving sexp_of]


let subscribe =
  conv
    (fun { reqid ; pairs ; sub } ->
       ((), reqid, List.map Pair.to_string pairs, sub))
    (fun ((), reqid, pair, sub) ->
       let pairs = List.map Pair.of_string_exn pair in
       { reqid ; pairs ; sub })
    (obj4
     (req "event" (constant "subscribe"))
     (opt "reqid" int)
     (dft "pair" (list string) [])
     (req "subscription" subscription))

let subscriptionStatus =
  string_enum [
    "subscribed", `Subscribed ;
    "unsubscribed", `Unsubscribed ;
  ]

type subscription_status = {
  chanid : int option ;
  name : string ;
  pair : Pair.t option ;
  status : [`Subscribed|`Unsubscribed] ;
  reqid : int option ;
  subscription : subscription ;
} [@@deriving sexp_of]

let subscription_status =
  conv
    (fun { chanid ; name ; pair ; status ; reqid ; subscription } ->
       ((), reqid, status, chanid, name, pair, subscription))
    (fun ((), reqid, status, chanid, name, pair, subscription) ->
       { reqid ; status ; chanid ; name ; pair ; subscription })
  (obj7
     (req "event" (constant "subscriptionStatus"))
     (opt "reqid" int)
     (req "status" subscriptionStatus)
     (opt "channelID" int)
     (req "channelName" string)
     (opt "pair" Pair.encoding)
     (req "subscription" subscription))

type subscription_error = {
  reqid : int option ;
  msg : string
} [@@deriving sexp_of]

let subscription_error =
  conv
    (fun _ -> assert false)
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
} [@@deriving sexp_of]

let unsubscribe =
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
} [@@deriving sexp_of]

let a_encoding =
  conv
    (fun { price; wholeLotVolume; lotVolume} -> ( price, wholeLotVolume, lotVolume ))
    (fun ( price, wholeLotVolume, lotVolume) -> { price; wholeLotVolume; lotVolume })
    (tup3 strfloat int strfloat)

type b = {
  price: float ;
  wholeLotVolume : int ;
  lotVolume : float
} [@@deriving sexp_of]

let b_encoding =
  conv
    (fun { price; wholeLotVolume; lotVolume} -> ( price, wholeLotVolume, lotVolume ))
    (fun ( price, wholeLotVolume, lotVolume) -> { price; wholeLotVolume; lotVolume })
    (tup3 strfloat int strfloat)

type c = {
  price: float ;
  lotVolume : float
} [@@deriving sexp_of]

let c_encoding =
  conv
    (fun { price; lotVolume} -> ( price, lotVolume ))
    (fun ( price, lotVolume) -> { price; lotVolume })
    (tup2 strfloat strfloat)

type v = {
  today: float ;
  last24Hours: float
} [@@deriving sexp_of]

let v_encoding =
  conv
    (fun { today; last24Hours } -> ( today, last24Hours ))
    (fun ( today, last24Hours ) -> { today; last24Hours })
    (tup2 strfloat strfloat)

type p = {
  today: float ;
  last24Hours: float
} [@@deriving sexp_of]

let p_encoding =
  conv
    (fun { today; last24Hours } -> ( today, last24Hours ))
    (fun ( today, last24Hours ) -> { today; last24Hours })
    (tup2 strfloat strfloat)

type ti = {
  today: int ;
  last24Hours: int
} [@@deriving sexp_of]

let ti_encoding =
  conv
    (fun { today; last24Hours } -> ( today, last24Hours ))
    (fun ( today, last24Hours ) -> { today; last24Hours })
    (tup2 int int)

type l = {
  today: float ;
  last24Hours: float
} [@@deriving sexp_of]

let l_encoding =
  conv
    (fun { today; last24Hours } -> ( today, last24Hours ))
    (fun ( today, last24Hours ) -> { today; last24Hours })
    (tup2 strfloat strfloat)

type h = {
  today: float ;
  last24Hours: float
} [@@deriving sexp_of]

let h_encoding =
  conv
    (fun { today; last24Hours } -> ( today, last24Hours ))
    (fun ( today, last24Hours ) -> { today; last24Hours })
    (tup2 strfloat strfloat)

type o = {
  today: float ;
  last24Hours: float
} [@@deriving sexp_of]

let o_encoding =
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
} [@@deriving sexp_of]

let ticker_encoding =
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
} [@@deriving sexp_of]

let side_encoding =
  string_enum [
    "b", Fixtypes.Side.Buy ;
    "s", Sell ;
  ]

let ord_type_encoding =
  string_enum [
    "l", Fixtypes.OrdType.Limit ;
    "m", Market ;
  ]

let trade_encoding =
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
} [@@deriving sexp_of]

let book_entry_encoding =
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
} [@@deriving sexp_of]

let snap_encoding =
  conv
    (fun { asks ; bids } -> (asks, bids))
    (fun (asks, bids) -> { asks ; bids })
    (obj2
       (req "bs" (list book_entry_encoding))
       (req "as" (list book_entry_encoding)))

let book_encoding =
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
  | SubscriptionError of subscription_error
  | SubscriptionStatus of subscription_status
  | Ticker of ticker update
  | Trade of trade list update
  | Snapshot of book update
  | Quotes of book update

  | OwnTrades of Trade.t list
  | OpenOrders of Order.t list

and 'a update = {
  chanid: int ;
  feed: string ;
  pair: Pair.t ;
  data: 'a ;
}
[@@deriving sexp_of]


let ownTrades ?reqid token = Subscribe { reqid; pairs = []; sub = OwnTrades token }
let openOrders ?reqid token = Subscribe { reqid; pairs = []; sub = OpenOrders token }
let tickers ?reqid pairs = Subscribe { reqid ; pairs ; sub = Ticker }
let trades ?reqid pairs = Subscribe { reqid ; pairs ; sub = Trade }
let book10 ?reqid pairs = Subscribe { reqid ; pairs ; sub = Book 10 }
let book25 ?reqid pairs = Subscribe { reqid ; pairs ; sub = Book 25 }
let book100 ?reqid pairs = Subscribe { reqid ; pairs ; sub = Book 100 }
let book500 ?reqid pairs = Subscribe { reqid ; pairs ; sub = Book 500 }
let book1000 ?reqid pairs = Subscribe { reqid ; pairs ; sub = Book 1000 }

let full_book_update_encoding =
  conv
    (fun { chanid ; feed ; pair ; data = { asks ; bids } } ->
       (chanid, { asks ; bids = [] }, { asks = [] ; bids }, feed, pair))
    (fun (chanid, { asks ; _ }, { bids ; _ }, feed, pair) ->
       { chanid ; feed ; pair ; data = { asks ; bids } })
    (tup5 int book_encoding book_encoding string Pair.encoding)

let update_encoding enc =
  conv
    (fun { chanid; feed; pair; data } -> (chanid, data, feed, pair))
    (fun (chanid, data, feed, pair) -> { chanid; feed; pair; data })
    (tup4 int enc string Pair.encoding)

let pp ppf t =
  Format.fprintf ppf "%a" Sexplib.Sexp.pp_hum (sexp_of_t t)

let ping =
  conv
    (fun reqid -> ((), reqid))
    (fun ((), reqid) -> reqid)
    (obj2
       (req "event" (constant "ping"))
       (opt "reqid" int))

let pong =
  conv
    (fun reqid -> ((), reqid))
    (fun ((), reqid) -> reqid)
    (obj2
       (req "event" (constant "pong"))
       (opt "reqid" int))

let hb =
  obj1 (req "event" (constant "heartbeat"))

let ownTrades_enc =
  conv
    (fun _ -> assert false)
    (fun (ts,()) -> List.concat ts)
    (tup2 (list (kraklist Trade.encoding KrakID.of_string)) (constant "ownTrades"))

let openOrders_enc =
  conv
    (fun _ -> assert false)
    (fun (os,()) -> List.concat os)
    (tup2 (list (kraklist Order.encoding KrakID.of_string)) (constant "openOrders"))

let encoding =
  union [
    case ping (function Ping reqid -> Some reqid | _ -> None) (fun reqid -> Ping reqid) ;
    case pong (function Pong reqid -> Some reqid | _ -> None) (fun reqid -> Pong reqid) ;
    case hb (function HeartBt -> Some () | _ -> None) (fun () -> HeartBt) ;
    case status (function Status s -> Some s | _ -> None) (fun s -> Status s) ;
    case subscription_error (function SubscriptionError e -> Some e | _ -> None) (fun e -> SubscriptionError e) ;
    case subscription_status (function SubscriptionStatus s -> Some s | _ -> None) (fun s -> SubscriptionStatus s) ;
    case subscribe (function Subscribe v -> Some v | _ -> None) (fun v -> Subscribe v) ;
    case unsubscribe (function Unsubscribe v -> Some v | _ -> None) (fun v -> Unsubscribe v) ;
    case (update_encoding ticker_encoding) (function Ticker t -> Some t | _ -> None) (fun t -> Ticker t) ;
    case (update_encoding (list trade_encoding)) (function Trade t -> Some t | _ -> None) (fun t -> Trade t) ;
    case (update_encoding snap_encoding) (function Snapshot s -> Some s | _ -> None) (fun s -> Snapshot s) ;
    case (update_encoding book_encoding) (function Quotes b -> Some b | _ -> None) (fun b -> Quotes b) ;
    case full_book_update_encoding (function Quotes b -> Some b | _ -> None) (fun b -> Quotes b) ;

    case ownTrades_enc (function OwnTrades ts -> Some ts | _ -> None) (fun ts -> OwnTrades ts) ;
    case openOrders_enc (function OpenOrders os -> Some os | _ -> None) (fun os -> OpenOrders os) ;
  ]

let of_string msg =
  Ezjsonm_encoding.destruct_safe encoding (Ezjsonm.from_string msg)

let to_string t =
  match Ezjsonm_encoding.construct encoding t with
  | `A _ | `O _ as a -> Ezjsonm.to_string a
  | #Json_repr.ezjsonm -> invalid_arg "not a json document"

