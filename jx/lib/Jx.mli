(** External JavaScript language interface for OCaml.

    This module provides types and functions to allow compile-time and runtime
    interoperability between JavaScript and OCaml. *)

type +'c obj constraint 'c = [> ]
(** Typed JavaScript objects.

    The type parameter ['c] is used to differentiate between objects of
    different classes using polymorphic variants.

    For example, the JavaScript
    {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date}
      Date} class can be represented as: [[ `Date ] Js.obj]. *)

type any = [ `Any ] obj
(** Opaque JavaScript objects.

    The [any] type is used to represent arbitrary JavaScript values whose static
    type information is unknown. This is useful for interfacing with low-level
    JavaScript APIs.

    The JavaScript {!type:any} values can be converted to and from OCaml values
    using conversion functions below. *)

type prop = string
(** JavaScript object property names.

    {e Note:} Properties names with non-ASCII characters require transcoding
    (see {!section:unicode}). *)

(** {2 Debug} *)

val log : 'a -> unit
(** Print the runtime representation of a value to the console. See
    {{:https://developer.mozilla.org/en-US/docs/Web/API/console/log_static}
      console.log}.*)

external debugger : unit -> unit = "debugger"
(** See
    {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/debugger}
      debugger}. *)

(** {2 Global} *)

val global : any
(** See
    {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/globalThis}
      globalThis}.

    {[
      let window : [ `Window ] Jx.obj = Jx.get Jx.global "window"
    ]} *)

(** {2:bindings Bindings}

    Bindings are definitions that allow accessing JavaScript functions and
    classes from OCaml.

    The following example uses the {!module:Encode} and {!module:Decode} modules
    to provide bindings for the builtin
    {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/parseInt}
      parseInt} JavaScript function:

    {[
      module E = Jx.Encode
      module D = Jx.Decode

      (* val parse_int : radix:int -> string -> int *)
      let parse_int ~radix str =
        D.int (D.func (Jx.expr "parseInt") [| E.string str; E.int radix |])

      let () = Jx.log (parse_int ~radix:10 "42")
    ]}

    In the above example, {!Jx.expr} is used to obtain a reference to the
    JavaScript [parseInt] function. It is then decoded as a function and called
    with an array of JavaScript-encoded values. Finally, the return value is
    decoded as an integer.

    After compilation, the following JavaScript code should be produced (in
    addition to the OCaml runtime):

    {@javascript[
      var x = parseInt("42", 10);
      console.log(x)
    ]} *)

(** {3:encode Encode}

    Encode OCaml values into opaque JavaScript values. *)

module Encode : sig
  val unit : unit -> any
  (** Encode an OCaml unit value as a JavaScript [undefined] value. *)

  external bool : bool -> any = "caml_js_from_bool"
  (** Encode an OCaml bool value. *)

  external int : int -> any = "%identity"
  (** Encode an OCaml int value. *)

  external char : char -> any = "%identity"
  (** Encode an OCaml char value. *)

  external float : float -> any = "caml_js_from_float"
  (** Encode an OCaml float value. *)

  external unicode : Stdlib.String.t -> any = "caml_jsstring_of_string"
  (** Encode an OCaml UTF-8 string as a JavaScript UTF-16 string. *)

  external string : Stdlib.String.t -> any = "%identity"
  (** Encode an OCaml ASCII string as a JavaScript string. *)

  val array : ('a -> any) -> 'a Stdlib.Array.t -> any
  (** Encode an OCaml array as JavaScript array. *)

  external obj_array : 'a obj Stdlib.Array.t -> any = "caml_js_from_array"
  (** Like {!val:array} but specialized for arrays of JavaScript objects. *)

  external any_array : 'a Stdlib.Array.t -> any = "caml_js_from_array"
  (** Like {!val:array} but specialized for arrays generic values. *)

  val list : ('a -> any) -> 'a Stdlib.List.t -> any
  (** Encode an OCaml list as JavaScript array. *)

  external obj_list : 'a obj Stdlib.List.t -> any = "caml_list_to_js_array"
  (** Like {!val:list} but specialized for lists of JavaScript objects. *)

  external any_list : 'a Stdlib.List.t -> any = "caml_list_to_js_array"
  (** Like {!val:list} but specialized for lists of generics values. *)

  val nullable : ('a -> any) -> 'a option -> any
  (** Encode an OCaml option as a JavaScript value that can be [null]. *)

  val obj_nullable : 'a obj option -> any
  (** Like {!val:nullable} but specialized for options of JavaScript objects. *)

  val any_nullable : 'a option -> any
  (** Like {!val:nullable} but specialized for options of generic values. *)

  val undefined : ('a -> any) -> 'a option -> any
  (** Encode an OCaml option as JavaScript value that can be [undefined]. *)

  val obj_undefined : 'a obj option -> any
  (** Like {!val:undefined} but specialized for options of JavaScript objects. *)

  val any_undefined : 'a option -> any
  (** Like {!val:undefined} but specialized for options of generic values. *)

  val field : 'c obj -> prop -> ('a -> any) -> 'a -> unit
  (** Encode and set a field value. *)

  external func : int -> (_ -> _) -> any = "caml_js_wrap_callback_strict"
  (** Encode an OCaml function of a given arity as a JavaScript function. *)

  external any : 'a -> any = "%identity"
  (** Encode a generic value. *)

  external obj : 'a obj -> any = "%identity"
  (** Encode a JavaScript object. *)

  val dict : ('a -> any) -> (prop * 'a) Stdlib.Array.t -> any
  (** Encode an array of key-value pairs as a homogeneous JavaScript object. *)

  external obj_dict : (prop * 'a obj) Stdlib.Array.t -> any = "caml_js_object"
  (** Like {!val:dict} but specialized for values that are JavaScript object. *)

  external any_dict : (prop * 'a) Stdlib.Array.t -> any = "caml_js_object"
  (** Like {!val:dict} but specialized for generic values. *)

  module Array : sig
    external obj : 'a obj Stdlib.Array.t -> any Stdlib.Array.t = "%identity"
    (** Encode an OCaml array of JavaScript objects as an OCaml array of
        JavaScript any values. *)

    external any : 'a Stdlib.Array.t -> any Stdlib.Array.t = "%identity"
    (** Encode an OCaml array of generic values as an OCaml array of JavaScript
        any values. *)
  end
end

(** {3:decode Decode}

    Decode OCaml values from opaque JavaScript values. *)

module Decode : sig
  val unit : any -> unit
  (** Decode an OCaml unit value. *)

  external bool : any -> bool = "caml_js_to_bool"
  (** Decode an OCaml bool value. *)

  external int : any -> int = "%identity"
  (** Decode an OCaml int value. *)

  external float : any -> float = "caml_js_to_float"
  (** Decode an OCaml float value. *)

  external unicode : any -> Stdlib.String.t = "caml_string_of_jsstring"
  (** Decode an OCaml UTF-8 string from a JavaScript UTF-16 string. *)

  external string : any -> Stdlib.String.t = "%identity"
  (** Decode an OCaml ASCII string from a JavaScript string. *)

  val array : (any -> 'a) -> any -> 'a Stdlib.Array.t
  (** Decode an OCaml array from a JavaScript array. *)

  external obj_array : any -> 'a obj Stdlib.Array.t = "caml_js_to_array"
  (** Like {!val:array} but specialized for arrays of JavaScript objects. *)

  val list : (any -> 'a) -> any -> 'a Stdlib.List.t
  (** Decode an OCaml list from a JavaScript array. *)

  external obj_list : any -> 'a obj Stdlib.List.t = "caml_list_of_js_array"
  (** Like {!val:list} but specialized for lists of JavaScript objects. *)

  val nullable : (any -> 'a) -> any -> 'a option
  (** Decode an OCaml option from a JavaScript value that can be [null]. *)

  val obj_nullable : any -> 'a obj option
  (** Like {!val:nullable} but specialized for options of JavaScript objects. *)

  val undefined : (any -> 'a) -> any -> 'a option
  (** Decode an OCaml option from a JavaScript value that can be [undefined]. *)

  val obj_undefined : any -> 'a obj option
  (** Like {!val:undefined} but specialized for options of JavaScript objects. *)

  val field : 'c obj -> prop -> (any -> 'a) -> 'a
  (** Decode an object field. *)

  external func : any -> any Stdlib.Array.t -> any = "caml_js_fun_call"
  (** Decode an OCaml function from a JavaScript function. *)

  external meth : 'a obj -> prop -> any Stdlib.Array.t -> any
    = "caml_js_meth_call"
  (** Decode a JavaScript method as an OCaml function. *)

  external obj : any -> 'a obj = "%identity"
  (** Decode a generic JavaScript object. *)
end

(** {3:raw Raw JavaScript}

    The {!expr} and {!stmt} functions can be used to embed untyped JavaScript
    code into the compiled application.

    The textual representation of the code must be valid JavaScript, otherwise
    the compilation will fail.

    The provided JavaScript code must be represented as a static string literal.
    If the provided code string is computed dynamically, the evaluation will
    fallback to runtime and an error will be thrown (check the console for
    evaluation errors).

    {b Warning:} Using {!expr} and {!stmt} is extremely unsafe since no
    type-checking is performed by the compiler on the embedded JavaScript code.
    Use this feature with special care! *)

external expr : string -> 'c obj = "caml_pure_js_expr"
(** Unsafe JavaScript expression.

    {[
      assert (Jx.Decode.int (Jx.expr "2 + 2") = 4)
    ]} *)

external stmt : string -> unit = "caml_js_expr"
(** Unsafe JavaScript statement.

    {[
      let () = Jx.stmt "console.log('hello')"
    ]} *)

(** {2 Unicode}

    Use {!val:unicode} when passing a Unicode string from OCaml to JavaScript.

    {[
      Jx.log (Jx.unicode "Olá, OCaml! 🐫")
    ]}

    This is needed because the Unicode representations in JavaScript and OCaml
    are different:

    - Strings in JavaScript use the UTF-16 Unicode encoding.
    - Strings in OCaml, while being arbitrary sequences of bytes, normally use
      the UTF-8 Unicode encoding.

    This means that an OCaml Unicode string is {e not} a valid JavaScript
    Unicode string. Transcoding functions between UTF-8 and UTF-16 are provided
    below.

    {e Note:} Regular ASCII strings do not require transcoding.

    See
    {{:https://ocaml.org/manual/4.12/api/String.html} String in The OCaml
      Manual}.

    See
    {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String}
      String on MDN}. *)

external utf16 : string -> string = "caml_jsstring_of_string"
(** Transcode a string from UTF-8 to UTF-16. *)

external utf8 : string -> string = "caml_string_of_jsstring"
(** Transcode a string from UTF-16 to UTF-8. *)

external unicode : string -> string = "caml_jsstring_of_string"
(** Transcode a string from UTF-8 to UTF-16. This is an alias for {!val:utf16}. *)

(** {2 Types} *)

(** {3 Object} *)

external get : 'c obj -> prop -> any = "caml_js_get"
external set : 'c obj -> prop -> 'v obj -> unit = "caml_js_set"
external del : 'c obj -> prop -> unit = "caml_js_delete"

external obj : (prop * any) Stdlib.Array.t -> 'c obj = "caml_js_object"
(** [obj [| (prop1, v1); ... |]] is [{prop1: v1, ... }]. *)

external obj_new : 'c obj -> any Stdlib.Array.t -> 'a obj = "caml_js_new"
(** [obj_new obj []] is [new obj(...args)]. *)

external typeof : 'c obj -> Stdlib.String.t = "caml_js_typeof"
(** See
    {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/typeof}
      typeof}. *)

external instanceof : 'c obj -> constr:'constr obj -> bool
  = "caml_js_instanceof"
(** See
    {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/instanceof}
      instanceof}. *)

external equal : 'c obj -> 'c obj -> bool = "caml_js_equals"
(** See
    {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Equality}
      Equality (==)}. *)

external strict_equal : 'c obj -> 'c obj -> bool = "caml_js_strict_equals"
(** See
    {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Strict_equality}
      Strict equality (===)}. *)

(** {3 Nullable} *)

type +'a nullable = [ `Nullable of 'a ] obj

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

(** {3 Undefined} *)

type +'a undefined = [ `Undefined of 'a ] obj

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

(** {3 Boolean} *)

type boolean = [ `Boolean ] obj

val true' : boolean
val false' : boolean
external bool : Stdlib.Bool.t -> boolean = "caml_js_from_bool"
external to_bool : boolean -> Stdlib.Bool.t = "caml_js_to_bool"

(** {3 Number} *)

type number = [ `Number ] obj

external int : Stdlib.Int.t -> number = "%identity"
external to_int : number -> Stdlib.Int.t = "%identity"
external float : Stdlib.Float.t -> number = "caml_js_from_float"
external to_float : number -> Stdlib.Float.t = "caml_js_to_float"
external int32 : Int32.t -> number = "caml_js_from_int32"
external to_int32 : number -> Int32.t = "caml_js_to_int32"
external nativeint : nativeint -> number = "caml_js_from_nativeint"
external to_nativeint : number -> nativeint = "caml_js_to_nativeint"

(** {3 String} *)

module String : sig
  type t = [ `String ] obj
end

external string : string -> String.t = "%identity"
external to_string : String.t -> string = "%identity"

(** {3 Array} *)

type +'a array = [ `Array of 'a ] obj

external array : 'a Stdlib.Array.t -> 'a array = "caml_js_from_array"
external to_array : 'a array -> 'a Stdlib.Array.t = "caml_js_to_array"
external list : 'a list -> 'a array = "caml_list_to_js_array"
external to_list : 'a array -> 'a list = "caml_list_of_js_array"
