(** A thin bridge between the JS and ML worlds. *)

type any
(** An opaque JavaScript value. *)

type +'cls obj constraint 'cls = [> ]
(** JavaScript object values. The type parameter ['cls] is used to differentiate
    between objects of different classes using polymorphic variants.

    For example, the JavaScript
    {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date}
      Date} class can be represented as [[`Date] Js.obj]. *)

(** {2 Nullable} *)

type +'a nullable
(** The type-safe encoding of JavaScript values that can be null. *)

val null : 'a nullable
val nullable : 'a -> 'a nullable
val is_null : 'a nullable -> bool

module Nullable : sig
  type +'a t = 'a nullable

  val of_option : 'a option -> 'a t
  val to_option : 'a t -> 'a option
  val get : 'a t -> 'a
  val map : ('a -> 'b) -> 'a t -> 'b t
end

(** {2 Undefined} *)

type +'a undefined

val undefined : 'a undefined
val defined : 'a -> 'a undefined
val is_undefined : 'a undefined -> bool
val is_defined : 'a undefined -> bool

module Undefined : sig
  type +'a t = 'a undefined

  val of_option : 'a option -> 'a t
  val to_option : 'a t -> 'a option
  val get : 'a t -> 'a
  val map : ('a -> 'b) -> 'a t -> 'b t
end

(*
  Target.Encode.source : source -> target
  Target.Encode.specific_source : source -> target
  Target.Encode.source_as_target_variant : source -> target
*)

module Encode : sig
  external js : any -> any = "%identity"
  (** Identity encoder. *)

  external any : 'a -> any = "%identity"
  (** Encode any value without performing runtime conversion. *)

  external obj : 'a obj -> any = "%identity"
  (** Encode an ML representation of a JS object as a JS value. *)

  external int : int -> any = "%identity"
  (** Encode an ML integer as a JS number. *)

  external char : char -> any = "%identity"
  (** Encode an ML character as a JS number. *)

  external unit : unit -> any = "%identity"
  (** Encode an ML unit value as the JS [undefined] value. *)

  external bool : bool -> any = "caml_js_from_bool"
  (** Encode an ML boolean as a JS boolean. *)

  external float : float -> any = "caml_js_from_float"
  (** Encode an ML float as a JavaScript number. *)

  external string : string -> any = "caml_jsstring_of_string"
  (** Encode an arbitrary ML string as a JavaScript UTF-16 string. *)

  external string_ascii : string -> any = "%identity"
  (** Encode an ML ASCII string as a JavaScript UTF-16 string. *)

  val array : ('a -> any) -> 'a array -> any
  (** Encode an ML array as a JS array, converting the array values using the
      provided encoder. *)

  external js_array : any array -> any = "caml_js_from_array"
  (** Encode an ML array of JS values as a JS array. The array values are not
      converted. *)

  val nullable : ('a -> any) -> 'a nullable -> any
  (** Encode an ML nullable as a JS nullable value. *)

  external js_nullable : any nullable -> any = "%identity"
  (** Encode an ML nullable of a JS value as a JS value that can be null. The
      contained value is not converted. *)

  val undefined : ('a -> any) -> 'a undefined -> any
  (** Encode an ML undefined as a JS value that can be undefined. *)

  external js_undefined : any undefined -> any = "%identity"
  (** Encode an ML undefined of a JS value as a JS value that can be undefined.
      The contained value is not converted. *)

  val option_as_nullable : ('a -> any) -> 'a option -> any
  (** Encode an ML option as a JS value that can be null. The contained value is
      converted with the provided encoder. *)

  val option_as_undefined : ('a -> any) -> 'a option -> any
  (** Encode an ML option as a JS value that can be undefined. The contained
      value is converted with the provided encoder. *)

  external fun' : int -> (any -> _) -> any = "caml_js_wrap_callback_strict"
  (** [fun' arity f] is the JavaScript representation of the OCaml function [f]
      with the given [arity]. *)
end

module Decode : sig
  external js : any -> any = "%identity"
  external any : any -> 'a = "%identity"
  external obj : any -> 'a obj = "%identity"
  external int : any -> int = "%identity"
  external unit : any -> unit = "%identity"
  external bool : any -> bool = "caml_js_to_bool"
  external float : any -> float = "caml_js_to_float"
  external string : any -> string = "caml_string_of_jsstring"
  external string_ascii : any -> string = "%identity"
  external js_array : any -> any array = "caml_js_to_array"
  val array : (any -> 'a) -> any -> 'a array
  val nullable : (any -> 'a) -> any -> 'a nullable
  val undefined : (any -> 'a) -> any -> 'a undefined
  val nullable_as_option : (any -> 'a) -> any -> 'a option
  val undefined_as_option : (any -> 'a) -> any -> 'a option
end

(** {2 Function} *)

external fun_call : any -> any array -> any = "caml_js_fun_call"
(** [fun_call f args] is [f(...args)]. [f] is assumed to represent a JavaScript
    function. *)

(** {2 Object} *)

external get : 'a obj -> string -> any = "caml_js_get"
(** [get obj prop] is [obj[prop]]. *)

external set : 'a obj -> string -> any -> unit = "caml_js_set"
(** [set obj prop v] is [obj[prop] = v]. *)

external del : 'a obj -> string -> unit = "caml_js_delete"
(** [del obj prop] is [delete obj[prop]]. *)

external obj : (string * any) array -> 'a obj = "caml_js_object"
(** [obj [(prop1, v1); ...]] is [{prop1: v1, ... }]. *)

external obj_new : any -> any array -> 'a obj = "caml_js_new"
(** [obj_new obj args] is [new obj(...args)]. *)

external meth_call : 'a obj -> string -> any array -> any = "caml_js_meth_call"
(** [meth_call obj prop args] is [obj.prop(...args)]. *)

(** {2 Global} *)

val global_this : 'a obj
(** See
    {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/globalThis}
      globalThis}. *)

val global : string -> any
(** [global prop] is [get global_this prop], that is, [globalThis[prop]]. *)

(** {2 equal} *)

external equal : any -> any -> bool = "caml_js_equals"
(** See
    {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Equality}
      Equality (==)}. *)

external strict_equal : any -> any -> bool = "caml_js_strict_equals"
(** See
    {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Strict_equality}
      Strict equality (===)}. *)

(** {2 type} *)

external typeof : any -> string = "caml_js_typeof"
(** See
    {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/typeof}
      typeof}. *)

external instanceof : 'a obj -> constr:any -> bool = "caml_js_instanceof"
(** See
    {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/instanceof}
      instanceof}. *)

(** {2 debug} *)

val debug : 'a -> unit
(** See
    {{:https://developer.mozilla.org/en-US/docs/Web/API/console/debug_static}
      console.debug}.*)

external debugger : unit -> unit = "debugger"
(** See
    {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/debugger}
      debugger}. *)

(** {2 unsafe} *)

external raw : string -> any = "caml_pure_js_expr"
(** Unsafe pure JavaScript expression.

    Do {e not} use this for effectful expressions, if the resulting value is not
    used, it will be discarded from the compiled output. *)

(* external magic : 'a obj -> 'b obj = "%identity" *)
(* Unsafe conversion between JavaScript object values. *)
