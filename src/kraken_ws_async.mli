open Core
open Async

open Kraken_ws

exception Timeout

val with_connection :
  ?sandbox:bool ->
  ((Time_stamp_counter.t * t) Pipe.Reader.t ->
   t Pipe.Writer.t ->
   'a Deferred.t) -> 'a Deferred.t
(** [with_connection ?sandbox f]. Will raise [Timeout] if no message
    is received from Kraken in a 10sec time span. *)
