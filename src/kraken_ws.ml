open Sexplib.Std
open Kraken

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
  pair: string list ;
  sub: subscription ;
} [@@deriving sexp]

let subscribe_encoding =
  let open Json_encoding in
  conv
    (fun { reqid ; pair ; sub } -> ((), reqid, pair, sub))
    (fun ((), reqid, pair, sub) -> { reqid ; pair ; sub })
    (obj4
     (req "event" (constant "subscribe"))
     (opt "reqid" int)
     (req "pair" (list string))
     (req "subscription" subscription_encoding))

type subscription_status = {
  chanid : int ;
  status : subscriptionStatus ;
  pair : string ;
  reqid : int option ;
  subscription : subscription ;
} [@@deriving sexp]

let subscription_status_encoding =
  let open Json_encoding in
  conv
    (fun { chanid ; status ; pair ; reqid ; subscription } ->
       ((), reqid, status, chanid, pair, subscription))
    (fun ((), reqid, status, chanid, pair, subscription) ->
       { reqid ; status ; chanid ; pair ; subscription })
  (obj6
     (req "event" (constant "subscriptionStatus"))
     (opt "reqid" int)
     (req "status" subscriptionStatus_encoding)
     (req "channelID" int)
     (req "pair" string)
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

type trade = {
  price: float ;
  qty: float ;
  ts: Ptime.t ;
  side: [`buy|`sell] ;
  ord_type: [`market|`limit] ;
  misc: string ;
} [@@deriving sexp]

let side_encoding =
  let open Json_encoding in
  string_enum [
    "b", `buy ;
    "s", `sell ;
  ]

let ord_type_encoding =
  let open Json_encoding in
  string_enum [
    "l", `limit ;
    "m", `market ;
  ]

let msg_encoding encoding =
  let open Json_encoding in
  tup2 int (list encoding)

let msg'_encoding encoding =
  let open Json_encoding in
  tup2 int encoding

let trade_encoding =
  let open Json_encoding in
  conv
    (fun { price ; qty ; ts ; side ; ord_type ; misc } ->
       (price, qty, ts, side, ord_type, misc))
    (fun (price, qty, ts, side, ord_type, misc) ->
       { price ; qty ; ts ; side ; ord_type ; misc })
    (tup6 strfloat strfloat Ptime.encoding
       side_encoding ord_type_encoding string)

type book_entry = {
  price: float ;
  qty: float ;
  ts: Ptime.t ;
} [@@deriving sexp]

let book_entry_encoding =
  let open Json_encoding in
  conv
    (fun { price ; qty ; ts } -> (price, qty, ts))
    (fun (price, qty, ts) -> { price ; qty ; ts })
    (tup3 strfloat strfloat Ptime.encoding)

type book = {
  asks : book_entry list ;
  bids : book_entry list ;
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

let full_book_update_encoding =
  let open Json_encoding in
  conv
    (fun (i, { asks ; bids }) ->
       (i, { asks ; bids = [] }, { asks = [] ; bids }))
    (fun (i, { asks ; _ }, { bids ; _ }) -> (i, { asks ; bids }))
    (tup3 int book_encoding book_encoding)

type t =
  | Ping of int option
  | Pong of int option
  | HeartBt
  | Status of status
  | Subscribe of subscribe
  | Unsubscribe of unsubscribe
  | Error of error
  | SubscriptionStatus of subscription_status
  | Trade of int * trade list
  | Snapshot of int * book
  | BookUpdate of int * book
[@@deriving sexp]

let trade (i, t) = Trade (i, t)
let snapshot (i, t) = Snapshot (i, t)
let bupdate (i, t) = BookUpdate (i, t)

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
    case (msg_encoding trade_encoding) (function Trade (i, t) -> Some (i, t) | _ -> None) trade ;
    case (msg'_encoding snap_encoding) (function Snapshot (i, s) -> Some (i, s) | _ -> None) snapshot ;
    case (msg'_encoding book_encoding) (function BookUpdate (i, s) -> Some (i, s) | _ -> None) bupdate ;
    case full_book_update_encoding (function BookUpdate (i, s) -> Some (i, s) | _ -> None) bupdate ;
  ]
