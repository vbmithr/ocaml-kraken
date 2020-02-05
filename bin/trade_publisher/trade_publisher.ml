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
    start : unit Ivar.t ;
  }

  let create_state feed start = { feed; start }

  type parameters = unit
  type view = unit [@@deriving sexp_of]
  let view _ p = p
  let pp ppf t = Sexplib.Sexp.pp ppf (sexp_of_view t)
end
module A = Actor.Make(E)(R)(V)
include A

module Handlers : HANDLERS
  with type self = bounded queue t = struct
  type self = bounded queue t

  (* let process_event _self _w evt =
   *   match evt with
   *   | _ -> Deferred.unit *)

  let on_request :
    type a. 'b t -> a Request.t -> a Deferred.t = fun self (R.Ws _evt) ->
    let _st = state self in
    assert false

  let setup_feed_connection self launched =
    Feed.create
      ~server_name:"feed"
      ~on_event:begin function
        | Connected { r; w; _ } as e -> begin
            log_event self (Feed e) >>= fun () ->
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
         Deferred.Or_error.return Kraken_ws.url_public)

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

  let on_launch self _ () =
    let start = Ivar.create () in
    let feed = setup_feed_connection self start in
    return (V.create_state feed start)

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

let main () =
  let terminate = Ivar.create () in
  Signal.(handle terminating ~f:(fun _ ->
      Ivar.fill terminate ()
    )) ;
  launch
    ~base_name:["fh"] ~name:"kraken"
    (bounded 10)
    (Actor.Types.create_limits ())
    () (module Handlers) >>= fun ws_worker ->
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
        Logs_async_ovh.udp_or_systemd_reporter () >>> Logs.set_reporter ;
        main ()
    ] end |>
  Command.run
