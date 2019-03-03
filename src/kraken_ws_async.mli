open Core
open Async

open Kraken_ws

val with_connection :
  ?sandbox:bool ->
  ?heartbeat:Time_ns.Span.t ->
  (t Pipe.Reader.t -> t Pipe.Writer.t -> 'a Deferred.t) -> 'a Deferred.t
