open Async

open Kraken_ws

val connect :
  ?sandbox:bool -> unit ->
  (t Pipe.Reader.t * t Pipe.Writer.t * unit Deferred.t,
   [ `Internal of exn | `WS of Fastws_async.error ]) result Deferred.t

val connect_exn :
  ?sandbox:bool -> unit ->
  (t Pipe.Reader.t * t Pipe.Writer.t * unit Deferred.t) Deferred.t

val with_connection :
  ?sandbox:bool ->
  (t Pipe.Reader.t -> t Pipe.Writer.t -> 'a Deferred.t) ->
  ('a, [ `Internal of exn
       | `User_callback of exn
       | `WS of Fastws_async.error ]) result Deferred.t

val with_connection_exn : ?sandbox:bool ->
  (t Pipe.Reader.t -> t Pipe.Writer.t -> 'a Deferred.t) -> 'a Deferred.t
