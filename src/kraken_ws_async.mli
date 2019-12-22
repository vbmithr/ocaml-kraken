open Async
open Kraken_ws

val public : Uri.t
(** For public channels only. *)

val auth : Uri.t
(** For public and private channels, auth is required in all cases. *)

val beta : Uri.t
(** Beta endpoint, do not use in prod. *)

val connect :
  ?url:Uri.t -> unit ->
  (t Pipe.Reader.t * t Pipe.Writer.t * unit Deferred.t) Deferred.Or_error.t

val connect_exn :
  ?url:Uri.t -> unit ->
  (t Pipe.Reader.t * t Pipe.Writer.t * unit Deferred.t) Deferred.t

val with_connection :
  ?url:Uri.t ->
  (t Pipe.Reader.t -> t Pipe.Writer.t -> 'a Deferred.t) ->
  'a Deferred.Or_error.t

(* val with_connection_exn : ?beta:bool ->
 *   (t Pipe.Reader.t -> t Pipe.Writer.t -> 'a Deferred.t) -> 'a Deferred.t *)
