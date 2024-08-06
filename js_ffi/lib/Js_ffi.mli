(** A foreign function interface between JavaScript and OCaml. *)

(** {2 Types} *)

type +'c obj constraint 'c = [> ]
(** Typed JavaScript object values.

    The type parameter ['c] is used to differentiate between objects of
    different classes using polymorphic variants. For example, the JavaScript
    {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date}
      Date} class can be represented as:

    {[
      [ `Date ] Js.obj
    ]} *)

type any = [ `Any ] obj
(** Opaque JavaScript values.

    The [any] type is used to represent arbitrary JavaScript values whose static
    type information is unknown. This is useful for interfacing with low-level
    JavaScript APIs.

    The JavaScript {!type:any} values can be converted to and from OCaml values
    using conversion functions below. *)

type boolean = [ `Boolean ] obj
type number = [ `Number ] obj
type string = [ `String ] obj
type symbol = [ `Symbol ] obj
type +'a array = [ `Array of 'a ] obj
type function' = [ `Function ] obj
type +'a nullable = [ `Nullable of 'a ] obj
type +'a undefined = [ `Undefined of 'a ] obj
type 'a dict = [ `Dict of 'a ] obj
type 'a promise = [ `Promise of 'a ] obj

(** {2 Object} *)

external get : 'c obj -> Stdlib.String.t -> any = "caml_js_get"
external set : 'c obj -> Stdlib.String.t -> 'v obj -> unit = "caml_js_set"
external del : 'c obj -> Stdlib.String.t -> unit = "caml_js_delete"

external obj : (Stdlib.String.t * any) Stdlib.Array.t -> 'c obj
  = "caml_js_object"
(** [obj [| (prop1, v1); ... |]] is [{prop1: v1, ... }]. *)

external obj_new : 'c obj -> any Stdlib.Array.t -> 'a obj = "caml_js_new"
(** [obj_new obj []] is [new obj(...args)]. *)

external meth : 'a obj -> Stdlib.String.t -> any Stdlib.Array.t -> any
  = "caml_js_meth_call"
(** [meth_call obj prop args] is [obj.prop(...args)]. *)

(** {3 Type information} *)

external typeof : 'c obj -> Stdlib.String.t = "caml_js_typeof"
(** See
    {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/typeof}
      typeof}. *)

external instanceof : 'c obj -> constr:'constr obj -> bool
  = "caml_js_instanceof"
(** See
    {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/instanceof}
      instanceof}. *)

(** {3 Equality} *)

external equal : 'c obj -> 'c obj -> bool = "caml_js_equals"
(** See
    {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Equality}
      Equality (==)}. *)

external strict_equal : 'c obj -> 'c obj -> bool = "caml_js_strict_equals"
(** See
    {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Strict_equality}
      Strict equality (===)}. *)

(** {2 Debug} *)

val debug : 'a -> unit
(** See
    {{:https://developer.mozilla.org/en-US/docs/Web/API/console/debug_static}
      console.debug}.*)

val log : 'a -> unit
(** See
    {{:https://developer.mozilla.org/en-US/docs/Web/API/console/log_static}
      console.log}.*)

external debugger : unit -> unit = "debugger"
(** See
    {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/debugger}
      debugger}. *)

(** {2:any Any} *)

(* external any : 'c obj -> any = "%identity" *)
(* external any_array : 'a obj Stdlib.Array.t -> any Stdlib.Array.t = "%identity" *)

(** {2:conv Conversion} *)

(** {3 Boolean} *)

val true' : boolean
val false' : boolean

(* val is_true : boolean -> Stdlib.Bool.t
   val is_false : boolean -> Stdlib.Bool.t *)

external bool : Stdlib.Bool.t -> boolean = "caml_js_from_bool"
external to_bool : boolean -> Stdlib.Bool.t = "caml_js_to_bool"

(** {3 Number} *)

external int : Stdlib.Int.t -> number = "%identity"
external to_int : number -> Stdlib.Int.t = "%identity"
external float : Stdlib.Float.t -> number = "caml_js_from_float"
external to_float : number -> Stdlib.Float.t = "caml_js_to_float"
external int32 : Int32.t -> number = "caml_js_from_int32"
external to_int32 : number -> Int32.t = "caml_js_to_int32"
external nativeint : nativeint -> number = "caml_js_from_nativeint"
external to_nativeint : number -> nativeint = "caml_js_to_nativeint"

(** {3 String} *)

external ascii : Stdlib.String.t -> string = "%identity"
external to_string_ascii : string -> Stdlib.String.t = "%identity"
external unicode : Stdlib.String.t -> string = "caml_jsstring_of_string"

external to_string_unicode : string -> Stdlib.String.t
  = "caml_string_of_jsstring"

external utf16_of_utf8 : Stdlib.String.t -> Stdlib.String.t
  = "caml_jsstring_of_string"

external utf8_of_utf16 : Stdlib.String.t -> Stdlib.String.t
  = "caml_string_of_jsstring"

(** {3 Array} *)

external array : 'a Stdlib.Array.t -> 'a array = "caml_js_from_array"
external to_array : 'a array -> 'a Stdlib.Array.t = "caml_js_to_array"
external list : 'a list -> 'a array = "caml_list_to_js_array"
external to_list : 'a array -> 'a list = "caml_list_of_js_array"

(** {2 Nullable} *)

val null : 'a nullable
external nullable : 'a -> 'a nullable = "%identity"
val is_null : 'a nullable -> bool

(** The type-safe encoding of JavaScript values that can be null. *)

module Nullable : sig
  type +'a t = 'a nullable

  val of_any : (any -> 'a) -> any -> 'a t
  val to_any : ('a -> any) -> 'a t -> any
  val of_option : 'a option -> 'a t
  val to_option : 'a t -> 'a option
  val get : 'a t -> 'a
  external unsafe_get : 'a t -> 'a = "%identity"
  val map : ('a -> 'b) -> 'a t -> 'b t

  val map_or : 'b -> ('a -> 'b) -> 'a t -> 'b
  (** [map_or default f nullable] is [f (get nullable)] if [nullable] is {e not}
      {!val:null}, and [default] otherwise.

      {[
        Nullable.map_or 0 (fun x -> x + 1) nullable
      ]} *)

  val map_or_else : (unit -> 'b) -> ('a -> 'b) -> 'a t -> 'b
  (** {[
        Nullable.map_or (fun () -> 0) (fun x -> x + 1) nullable
      ]} *)
end

(* val nullable_of_any : any -> 'a nullable
   external any_of_nullable : 'a nullable -> any = "%identity" *)

(** {2 Undefined} *)

val undefined : 'a undefined
external defined : 'a -> 'a undefined = "%identity"
val is_undefined : 'a undefined -> bool
val is_defined : 'a undefined -> bool

module Undefined : sig
  type +'a t = 'a undefined

  val of_any : (any -> 'a) -> any -> 'a t
  val to_any : ('a -> any) -> 'a t -> any
  val of_option : 'a option -> 'a t
  val to_option : 'a t -> 'a option
  val get : 'a t -> 'a
  external unsafe_get : 'a t -> 'a = "%identity"
  val map : ('a -> 'b) -> 'a t -> 'b t

  val map_or : 'b -> ('a -> 'b) -> 'a t -> 'b
  (** {[
        Undefined.map_or 0 (fun x -> x + 1) undefined
      ]} *)

  val map_or_else : (unit -> 'b) -> ('a -> 'b) -> 'a t -> 'b
  (** {[
        Undefined.map_or (fun () -> 0) (fun x -> x + 1) undefined
      ]} *)
end

(** {2 Global} *)

val global : [ `Object ] obj
(** See
    {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/globalThis}
      globalThis}.

    {[
      let window : [ `Window ] Jx.obj = Jx.get Jx.global "window"
    ]} *)

(** {2 Unsafe} *)

external expr : Stdlib.String.t -> 'c obj = "caml_pure_js_expr"
(** Unsafe JavaScript expression.

    {[
      let x : Jx.number = Jx.expr "2 + 2"

      let () =
        let alert : Jx.function' = Jx.expr "window.alert" in
        ignore (Jx.call alert [| Jx.any (Jx.ascii "hello") |])
    ]} *)

external stmt : Stdlib.String.t -> unit = "caml_js_expr"
(** Unsafe JavaScript statement.

    {[
      Jx.stmt "console.log('hello')"
    ]} *)

(*
  Target.Encode.source : source -> target
  Target.Encode.specific_source : source -> target
  Target.Encode.source_as_target_variant : source -> target
*)

(* {2:conversion Conversion}

    The {!module:Encode} and {!module:Decode} modules can be used to convert
    between OCaml and JavaScript representations of values. This is particularly
    useful for defining bindings to JavaScript APIs.

    {3 Example}

    The following example defines bindings for the builtin
    {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/parseInt}
      parseInt} JavaScript function:

    {[
      module E = Js_ffi.Encode
      module D = Js_ffi.Decode

      let parse_int str radix =
        D.int (D.fun' (E.raw "parseInt") [| E.string str; E.int radix |])
    ]} *)

(* {3:encode Encode}

    Encode OCaml values to the equivalent JavaScript representation. *)

module Encode : sig
  external repr : 'a -> any = "%identity"
  external any : 'a obj -> any = "%identity"
  external any_array : 'a obj Stdlib.Array.t -> any Stdlib.Array.t = "%identity"
  external bool : bool -> any = "caml_js_from_bool"
  external int : int -> any = "%identity"
  val unit : unit -> any
  external char : char -> any = "%identity"
  external float : float -> any = "caml_js_from_float"
  external unicode : Stdlib.String.t -> any = "caml_jsstring_of_string"
  external string : Stdlib.String.t -> any = "%identity"
  val array : ('a -> any) -> 'a Stdlib.Array.t -> any
  external obj_array : 'a obj Stdlib.Array.t -> any = "caml_js_from_array"
  val list : ('a -> any) -> 'a Stdlib.List.t -> any
  external obj_list : 'a obj Stdlib.List.t -> any = "caml_list_to_js_array"
  val nullable : ('a -> any) -> 'a option -> any
  val undefined : ('a -> any) -> 'a option -> any

  external obj : (Stdlib.String.t * any) Stdlib.Array.t -> 'a obj
    = "caml_js_object"

  val dict : ('a -> any) -> (Stdlib.String.t * 'a) Stdlib.Array.t -> any
  external func : int -> (_ -> _) -> any = "caml_js_wrap_callback_strict"
end

(* {3:decode Decode}

    Decode OCaml values from the equivalent JavaScript representation. *)

module Decode : sig
  external any : any -> 'a obj = "%identity"
  external int : any -> int = "%identity"
  val unit : any -> unit
  external bool : any -> bool = "caml_js_to_bool"
  external float : any -> float = "caml_js_to_float"
  external unicode : any -> Stdlib.String.t = "caml_string_of_jsstring"
  external string : any -> Stdlib.String.t = "%identity"
  val array : (any -> 'a) -> any -> 'a Stdlib.Array.t
  external obj_array : any -> 'a obj Stdlib.Array.t = "caml_js_to_array"
  external any_array : any -> any Stdlib.Array.t = "caml_js_to_array"
  val list : (any -> 'a) -> any -> 'a Stdlib.List.t
  external any_list : any -> any Stdlib.List.t = "caml_list_of_js_array"
  val nullable : (any -> 'a) -> any -> 'a option
  val undefined : (any -> 'a) -> any -> 'a option
  external func : any -> any Stdlib.Array.t -> any = "caml_js_fun_call"

  (* val obj : any -> (string * any) Stdlib.Array.t *)

  (* val dict : (any -> 'v) -> any -> (string * 'v) Stdlib.Array.t *)
end
