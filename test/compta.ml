open Core
open Async

let src = Logs.Src.create "kraken.compta"
module Log_async = (val Logs_async.src_log src : Logs_async.LOG)

module Cfg = struct
  type cfg = {
    key: string ;
    secret: string ;
    passphrase: string [@default ""];
    quote: (string * int) list [@default []];
  } [@@deriving sexp]

  type t = (string * cfg) list [@@deriving sexp]
end

let default_cfg = Filename.concat (Option.value_exn (Sys.getenv "HOME")) ".virtu"
let cfg =
  List.Assoc.find_exn ~equal:String.equal
    (Sexplib.Sexp.load_sexp_conv_exn default_cfg Cfg.t_of_sexp) "KRAKEN"

open Kraken

let rec inner n =
  Fastrest.request
    ~auth:{ Fastrest.key = cfg.key ;
            secret = Base64.decode_exn cfg.secret ;
            meta = [] } (Kraken_rest.trade_history n) >>= function
  | Error e ->
    Log_async.err begin fun m ->
      m "%a" (Fastrest.pp_print_error Format.(pp_print_list pp_print_string)) e
    end
  | Ok fills ->
    let len = List.length fills in
    Logs_async.app (fun m -> m "Found %d fills" len) >>= fun () ->
    Deferred.List.iter fills ~f:begin fun (str, fill) ->
      Log_async.app (fun m -> m "%s %a" str Filled_order.pp fill)
    end >>= fun () ->
    if len < 50 then Deferred.unit
    else inner (n + len)

let () =
  Command.async ~summary:"Kraken kdb+ compta" begin
    let open Command.Let_syntax in
    [%map_open
      let () = Logs_async_reporter.set_level_via_param None in
      fun () ->
        Logs.set_reporter (Logs_async_reporter.reporter ()) ;
        inner 0
    ] end |>
  Command.run
