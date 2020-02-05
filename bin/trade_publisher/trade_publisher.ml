open Core
open Async
open Kraken
open Kraken_ws

module Feed =
  Persistent_connection_kernel.Make(
    Fastws_async.MakePersistent(struct
      type r = t type w = t
    end))

module E = struct
  type t =
    | Watchdog
    | EOD of Ptime.date
    | Feed of Feed.Event.t
    | Redis of Orewa.Persistent.Event.t
    | Snapshot of Pair.t
    | SubscriptionStatus of Kraken_ws.subscription_status
  [@@deriving sexp_of]

  let dummy = Watchdog
  let level = function
    | _ -> Logs.Info

  let pp ppf v = Sexplib.Sexp.pp ppf (sexp_of_t v)
end
module R = struct
  type _ t = Ws : Kraken_ws.t -> unit t
  let create ws = Ws ws
  type view = Kraken_ws.t [@@deriving sexp_of]
  let view : type a. a t -> Kraken_ws.t = function Ws ws -> ws
  let level _ = Logs.Debug
  let pp ppf v = Sexplib.Sexp.pp ppf (sexp_of_view v)
end
module V = struct
  type state = {
    feed  : Feed.t ;
    redis : Orewa.Persistent.t ;
    start : unit Ivar.t ;
    auth : Fastrest.auth ;
  }

  let create_state feed redis start auth = { feed; redis; start; auth }

  type parameters = {
    auth : Fastrest.auth ;
    redis_url: Uri.t ;
  }
  type view = unit [@@deriving sexp_of]
  let view _ _ = ()
  let pp ppf t = Sexplib.Sexp.pp ppf (sexp_of_view t)
end
module A = Actor.Make(E)(R)(V)
include A

module Handlers : HANDLERS
  with type self = bounded queue t = struct
  type self = bounded queue t

  let process_ws_event self evt =
    match evt with
    | OwnTrades trades -> begin
      let st = state self in
      match Orewa.Persistent.current_connection st.redis with
      | None -> Deferred.unit
      | Some c ->
        Deferred.List.iter trades ~f:begin fun t ->
          let msg =
            Ezjsonm.value_to_string (Ezjsonm_encoding.construct (Kraken.Trade.encoding t.id) t) in
          Deferred.ignore_m (Orewa.publish c "trades:kraken" msg)
        end
    end
    | _ -> Deferred.unit

  let on_request :
    type a. 'b t -> a Request.t -> a Deferred.t = fun self (R.Ws evt) ->
    process_ws_event self evt

  let setup_redis_connection self redis_url launched =
    Orewa.Persistent.create
      ~server_name:"redis"
      ~on_event:begin fun e ->
        log_event self (Redis e)
      end
      ~connect:begin fun url ->
        Async_uri.connect url >>= fun {r; w; _ } ->
        Deferred.Or_error.return (Orewa.create r w)
      end
      (fun () ->
         Ivar.read launched >>= fun () ->
         Deferred.Or_error.return redis_url)

  let setup_feed_connection self launched =
    let of_string s =
      Ezjsonm_encoding.destruct_safe Kraken_ws.encoding (Ezjsonm.value_from_string s) in
    let to_string t =
      Ezjsonm.value_to_string (Ezjsonm_encoding.construct Kraken_ws.encoding t) in
    Feed.create
      ~server_name:"feed"
      ~on_event:begin function
        | Connected { r; w; _ } as e -> begin
            let { V.feed = _; V.auth; _ } = state self in
            Fastrest.request ~auth Kraken_rest.websocket_token >>= fun { token; _ } ->
            log_event self (Feed e) >>= fun () ->
            Pipe.write w (ownTrades token) >>= fun () ->
            Pipe.close w ;
            don't_wait_for (Pipe.iter r ~f:(fun evt -> push_request self (Ws evt))) ;
            Deferred.unit
          end
        | e -> log_event self (Feed e)
      end
      ~connect:begin fun url ->
        Fastws_async.connect ~of_string ~to_string url >>=
        Deferred.Or_error.return
      end
      (fun () ->
         Ivar.read launched >>= fun () ->
         Deferred.Or_error.return Kraken_ws.url_auth)

  let on_close self =
    let { V.feed; _ } = state self in
    Feed.close feed

  let on_no_request self =
    log_event self Watchdog >>= fun () ->
    match Feed.current_connection (state self).feed with
    | None -> Deferred.unit
    | Some { r; w; _ } ->
      Pipe.close w ;
      Pipe.close_read r ;
      Deferred.unit

  let on_launch self _ { V.auth; redis_url } =
    let start = Ivar.create () in
    let feed = setup_feed_connection self start in
    let redis = setup_redis_connection self redis_url start in
    return (V.create_state feed redis start auth)

  let on_launch_complete self =
    let st = state self in
    Ivar.fill_if_empty st.start () ;
    Deferred.unit

  let on_completion _self _req _arg _status =
    Deferred.unit

  let on_error _self _view _status _error =
    Deferred.unit
end

let assert_subset set ~of_ =
  let open Pair.Set in
  if cardinal (inter set of_) < cardinal set then
    invalid_arg "symbol(s) unknown"

let fix_wsname { Pair.base; quote } =
  { Pair.base = String.(sub base ~pos:0 ~len:(pred (length base)));
    quote }

let key, secret =
  match String.split ~on:':' (Sys.getenv_exn "TOKEN_KRAKEN") with
  | [key; secret] -> key, secret
  | _ -> invalid_arg "invalid TOKEN_KRAKEN environment variable"

let redis_url =
  Option.value_map
    ~default:(Uri.make ~scheme:"tcp" ~host:"localhost" ~port:6379 ())
    (Sys.getenv "REDIS_URL")
    ~f:Uri.of_string

let auth =
  Fastrest.auth ~key ~secret:(Base64.decode_exn secret) ()

let main () =
  let terminate = Ivar.create () in
  Signal.(handle terminating ~f:(fun _ ->
      Ivar.fill terminate ()
    )) ;
  launch
    ~base_name:["fh"] ~name:"kraken"
    (bounded 10) (Actor.Types.create_limits ())
    { auth; redis_url } (module Handlers) >>= fun ws_worker ->
  Ivar.read terminate >>= fun () ->
  shutdown ws_worker

let sexp_of_uri uri =
  sexp_of_string (Uri.to_string uri)

let pair =
  Command.(Arg_type.map Param.string ~f:Pair.of_string_exn)

let () =
  Command.async ~summary:"Kraken Trade Publisher" begin
    let open Command.Let_syntax in
    [%map_open
      let () = Logs_async_reporter.set_level_via_param [] in
      fun () ->
        Logs.set_reporter (Logs_async_reporter.reporter ()) ;
        main ()
    ] end |>
  Command.run
