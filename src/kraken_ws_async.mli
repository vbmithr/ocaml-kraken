open Core
open Async

type t = {
  r: Kraken_ws.t Pipe.Reader.t ;
  w: Kraken_ws.t Pipe.Writer.t ;
}

val connect : Uri.t -> t Deferred.Or_error.t
val connect_exn : Uri.t -> t Deferred.t

val with_connection :
  f:(Kraken_ws.t Pipe.Reader.t ->
     Kraken_ws.t Pipe.Writer.t -> 'a Deferred.t) -> Uri.t ->
  'a Deferred.Or_error.t

module Persistent : sig
  include Persistent_connection_kernel.S
    with type address = Uri.t
     and type conn = t

  val create' :
    server_name:string ->
    ?on_event:(Event.t -> unit Deferred.t) ->
    ?retry_delay:(unit -> Time_ns.Span.t) ->
    (unit -> address Or_error.t Deferred.t) -> t
end
