open Core
open Async

open Kraken_ws

val connect :
  ?sandbox:bool -> unit ->
  ((Time_stamp_counter.t * t) Pipe.Reader.t *
   t Pipe.Writer.t * unit Deferred.t) Deferred.t

val with_connection :
  ?sandbox:bool ->
  ((Time_stamp_counter.t * t) Pipe.Reader.t -> t Pipe.Writer.t ->
   'a Deferred.t) -> 'a Deferred.t
