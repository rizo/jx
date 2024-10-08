(** External JavaScript interface for OCaml.

    This module provides bindings for standard JavaScript objects and external
    primitives that allow type-safe compile-time and runtime interoperability
    between JavaScript and OCaml.

    Start by exploring the standard JavaScript {!section:types} or learning
    about the {!section:bindings} API. *)

(** {1 Types}

    All standard JavaScript types are directly representable in OCaml without
    wrapping or any runtime conversions. This is achieved by providing
    semi-abstract types for global JavaScript objects types like
    {!section:number} and {!section:array}.

    When interacting with JavaScript APIs you can either use the specialized
    {!section:object} types for zero-cost access to values of that type, or you
    can write {!section:bindings} that convert OCaml values to JavaScript and
    vice-versa. *)

(** {2:object Object} *)

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
    using the {!module:Encode} and {!module:Decode} modules. *)

type prop = string
(** JavaScript object property names.

    {e Note:} Properties names with non-ASCII characters require transcoding
    (see {!section:unicode}). *)

external get : 'c obj -> prop -> 'v obj = "caml_js_get"
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

(** The JavaScript
    {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object}
      Object} class. *)
module Object : sig
  type 'a t = 'a obj
end

(** {2 Dict} *)

type +'a dict = [ `Dict of 'a ] obj
(** The type for JavaScript objects that act as containers for values of the
    same type. *)

external dict : (prop * 'a) Stdlib.Array.t -> 'a dict = "caml_js_object"
(** An object with homogeneous value types. *)

(** Operations on {!type:dict} values. *)
module Dict : sig
  type +'a t = 'a dict

  external make : (prop * 'a) Stdlib.Array.t -> 'a t = "caml_js_object"
  (** An object with homogeneous value types. *)
end

(** {2 Nullable}

    Type-safe encoding of JavaScript values that can be null. *)

type +'a nullable = [ `Nullable of 'a ] obj

val null : 'a nullable
external nullable : 'a -> 'a nullable = "%identity"
val is_null : 'a nullable -> bool

(* val nullable_of_any : any -> 'a nullable
   external any_of_nullable : 'a nullable -> any = "%identity" *)

(** Operations on {!type:nullable} values. *)
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
        Nullable.map_or_else (fun () -> 0) (fun x -> x + 1) nullable
      ]} *)
end

(** {2 Optional}

    Type-safe encoding of JavaScript values that can be [undefined]. Not to be
    confused with OCaml's standard [Option] type. *)

type +'a optional = [ `Optional of 'a ] obj

val undefined : 'a optional
external defined : 'a -> 'a optional = "%identity"
val is_undefined : 'a optional -> bool
val is_defined : 'a optional -> bool

(** Operations on {!type:optional} values. *)
module Optional : sig
  type +'a t = 'a optional

  val of_any : (any -> 'a) -> any -> 'a t
  val to_any : ('a -> any) -> 'a t -> any
  val of_option : 'a option -> 'a t
  val to_option : 'a t -> 'a option
  val get : 'a t -> 'a
  external unsafe_get : 'a t -> 'a = "%identity"
  val map : ('a -> 'b) -> 'a t -> 'b t

  val map_or : 'b -> ('a -> 'b) -> 'a t -> 'b
  (** {[
        Optional.map_or 0 (fun x -> x + 1) optional
      ]} *)

  val map_or_else : (unit -> 'b) -> ('a -> 'b) -> 'a t -> 'b
  (** {[
        Optional.map_or_else (fun () -> 0) (fun x -> x + 1) optional
      ]} *)
end

(** {2 Boolean}

    See
    {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Boolean}
      Boolean} on MDN.

    See {{:https://tc39.es/ecma262/#sec-boolean-objects} Boolean Objects} in
    ECMA262. *)

type boolean = [ `Boolean ] obj

external bool : Stdlib.Bool.t -> boolean = "caml_js_from_bool"
(** Create a JavaScript Boolean from an OCaml bool. *)

val true' : boolean
val false' : boolean

module Boolean : sig
  type t = boolean

  val true' : t
  val false' : t
  external of_bool : Stdlib.Bool.t -> t = "caml_js_from_bool"
  external to_bool : t -> Stdlib.Bool.t = "caml_js_to_bool"

  val to_string : t -> string
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Boolean/toString}
        [Boolean.toString] on MDN}. *)

  val value_of : t -> bool
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Boolean/valueOf}
        [Boolean.valueOf] on MDN}. *)
end

(** {2 Number} *)

type number = [ `Number ] obj
(** The JavaScript
    {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number}
      Number} type. *)

external int : Stdlib.Int.t -> number = "%identity"
(** Create a JavaScript {!type:number} from an OCaml int. *)

external float : Stdlib.Float.t -> number = "caml_js_from_float"
(** Create a JavaScript {!type:number} from an OCaml float. *)

external int32 : Stdlib.Int32.t -> number = "caml_js_from_int32"
(** Create a JavaScript {!type:number} from an OCaml int32. *)

external nativeint : Stdlib.Nativeint.t -> number = "caml_js_from_nativeint"
(** Create a JavaScript {!type:number} from an OCaml nativeint. *)

(** The JavaScript
    {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number}
      Number} class. *)
module Number : sig
  type t = number

  external of_int : Stdlib.Int.t -> t = "%identity"
  external to_int : t -> Stdlib.Int.t = "%identity"
  external of_float : Stdlib.Float.t -> t = "caml_js_from_float"
  external to_float : t -> Stdlib.Float.t = "caml_js_to_float"
  external of_int32 : Int32.t -> t = "caml_js_from_int32"
  external to_int32 : t -> Int32.t = "caml_js_to_int32"
  external of_nativeint : nativeint -> t = "caml_js_from_nativeint"
  external to_nativeint : t -> nativeint = "caml_js_to_nativeint"

  (** {2 Static methods} *)

  val is_finite : t -> bool
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Number/isFinite}
        [isFinite] on MDN}. *)

  val is_integer : t -> bool
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Number/isInteger}
        [isInteger] on MDN}. *)

  val is_nan : t -> bool
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Number/isNaN} [isNaN]
        on MDN}. *)

  val is_safe_integer : t -> bool
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Number/isSafeInteger}
        [isSafeInteger] on MDN}. *)

  val parse_float : Stdlib.String.t -> t
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Number/parseFloat}
        [parseFloat] on MDN}. *)

  val parse_int : string -> t
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Number/parseInt}
        [parseInt] on MDN}. *)

  val parse_int_with_radix : radix:int -> string -> t
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Number/parseInt}
        [parseInt] on MDN}. *)

  (** {2 Static properties} *)

  val epsilon : t
  val max_safe_integer : t
  val max_value : t
  val min_safe_integer : t
  val min_value : t
  val nan : t
  val negative_infinity : t
  val positive_infinity : t

  (** {2 Instance methods} *)

  val to_exponential : fraction_digits:int -> t -> Stdlib.String.t
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Number/toExponential}
        [toExponential] on MDN}. *)

  val to_fixed : fraction_digits:int -> t -> Stdlib.String.t
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Number/toFixed}
        [toFixed] on MDN}. *)

  val to_locale_string :
    locales:[< `String | `Array of [ `String ] ] obj -> t -> Stdlib.String.t
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Number/toLocaleString}
        [toLocaleString] on MDN}. *)

  val to_locale_string_with_options :
    locales:[< `String | `Array of [ `String ] ] obj ->
    options:any ->
    t ->
    Stdlib.String.t
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Number/toLocaleString}
        [toLocaleString] on MDN}. *)

  val to_precision : precision:int -> t -> Stdlib.String.t
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Number/toPrecision}
        [toPrecision] on MDN}. *)

  val to_string : radix:int -> t -> Stdlib.String.t
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Number/toString}
        [toString] on MDN}. *)

  val value_of : t -> t
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Number/valueOf}
        [valueOf] on MDN}. *)
end

(** {2 String} *)

type string = [ `String ] obj
(** The JavaScript
    {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String}
      String} type. *)

external string : Stdlib.String.t -> string = "%identity"
(** Create a JavaScript String from an OCaml string. *)

(** The JavaScript
    {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String}
      String} class. *)
module String : sig
  type t = string

  external of_string : Stdlib.String.t -> t = "%identity"
  external to_string : t -> Stdlib.String.t = "%identity"
end

(** {2 Symbol} *)

type symbol = [ `Symbol ] obj
(** The JavaScript
    {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Symbol}
      Symbol} type. *)

val symbol : Stdlib.String.t -> symbol
(** Create a
    {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Symbol/Symbol}
      Symbol} with a description. *)

module Symbol : sig
  type t = symbol

  val make : unit -> t
  (** Create a
      {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Symbol/Symbol}
        Symbol} using [Symbol()]. *)

  val with_description : Stdlib.String.t -> t
  (** Create a
      {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Symbol/Symbol}
        Symbol} with a description. *)

  val for_key : Stdlib.String.t -> t
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Symbol/for} [for] on
        MDN}. *)

  val key_for : t -> Stdlib.String.t
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Symbol/keyFor}
        [keyFor] on MDN}. *)

  val has_instance : t
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Symbol/hasInstance}
        [hasInstance] on MDN}. *)

  val is_concat_spreadable : t
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Symbol/isConcatSpreadable}
        [isConcatSpreadable] on MDN}. *)

  val iterator : t
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Symbol/iterator}
        [iterator] on MDN}. *)

  val match' : t
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Symbol/match} [match]
        on MDN}. *)

  val match_all : t
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Symbol/match}
        [matchAll] on MDN}. *)

  val replace : t
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Symbol/replace}
        [replace] on MDN}. *)

  val search : t
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Symbol/search}
        [search] on MDN}. *)

  val species : t
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Symbol/species}
        [species] on MDN}. *)

  val split : t
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Symbol/split} [split]
        on MDN}. *)

  val to_primitive : t
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Symbol/toPrimitive}
        [toPrimitive] on MDN}. *)

  val to_string_tag : t
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Symbol/toStringTag}
        [toStringTag] on MDN}. *)

  val unscopables : t
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Symbol/unscopables}
        [unscopables] on MDN}. *)

  val to_string : t -> Stdlib.String.t
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Symbol/toString}
        [toString] on MDN}. *)

  val value_of : t -> t
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Symbol/valueOf}
        [valueOf] on MDN}. *)
end

(** {2 Array} *)

type +'a array = [ `Array of 'a ] obj
(** The JavaScript
    {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array}
      Array} type. *)

(* external array : 'a Stdlib.Array.t -> 'a array = "caml_js_from_array" *)
val array : 'a Stdlib.Array.t -> 'a array
(** Create a JavaScript Array from an OCaml array. *)

external list : 'a Stdlib.List.t -> 'a array = "caml_list_to_js_array"
(** Create a JavaScript Array from an OCaml list. *)

(** The JavaScript
    {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array}
      Array} class. *)
module Array : sig
  type +'a t = 'a array

  external of_array : 'a Stdlib.Array.t -> 'a t = "caml_js_from_array"
  external to_array : 'a t -> 'a Stdlib.Array.t = "caml_js_to_array"
  external of_list : 'a Stdlib.List.t -> 'a t = "caml_list_to_js_array"
  external to_list : 'a t -> 'a Stdlib.List.t = "caml_list_of_js_array"
end

(** {2 Promise} *)

type +'a promise = [ `Promise of 'a ] obj
(** The JavaScript
    {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise}
      Promise} type. *)

module Promise : sig
  type +'a t = 'a promise
end

(** {2 BigInt} *)

type bigint = [ `Bigint ] obj
(** The JavaScript
    {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt}
      BigInt} type. *)

(** {2 Function} *)

type func = [ `Func ] obj
(** The JavaScript
    {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Function}
      Function} type. *)

module Func : sig
  type t = func

  val make : Stdlib.String.t Stdlib.Array.t -> t

  val apply : this:'a obj -> args:any array -> t -> any
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Function/apply}
        [apply] on MDN}. *)

  val bind : this:'a obj -> args:any array -> t -> t
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Function/bind} [bind]
        on MDN}. *)

  val call : this:'a obj -> args:any array -> t -> any
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Function/call} [call]
        on MDN}. *)

  val to_string : t -> Stdlib.String.t
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Function/toString}
        [toString] on MDN}. *)

  val length : t -> int
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Function/length}
        [length] on MDN}. *)

  val name : t -> Stdlib.String.t
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Function/name} [name]
        on MDN}. *)
end

(** {2 Iterator} *)

type 'a iterator = [ `Iterator of 'a ] obj
(** The JavaScript
    {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator}
      Iterator} type. *)

module Iterator : sig
  type 'a t = 'a iterator

  val make : unit -> 'a t
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator/Iterator}
        [Iterator] on MDN}. *)

  val from : _ obj -> 'a t
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator/from}
        [from] on MDN}. *)

  val drop : int -> 'a t -> 'a t
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator/drop}
        [drop] on MDN}. *)

  val every : ('a -> int -> bool) -> 'a t -> bool
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator/drop}
        [drop] on MDN}. *)
end

(** {1 Unicode}

    Use {!val:unicode} when passing a Unicode string from OCaml to JavaScript.

    {[
      Jx.log (Jx.unicode "Olá, OCaml! 🐫")
    ]}

    This is needed because the Unicode encoding in JavaScript and OCaml are
    different:

    - Strings in JavaScript use UTF-16.
    - Strings in OCaml, while being arbitrary sequences of bytes, typically use
      UTF-8.

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

external utf16 : Stdlib.String.t -> Stdlib.String.t = "caml_jsstring_of_string"
(** Transcode a string from UTF-8 to UTF-16. *)

external utf8 : Stdlib.String.t -> Stdlib.String.t = "caml_string_of_jsstring"
(** Transcode a string from UTF-16 to UTF-8. *)

external unicode : Stdlib.String.t -> Stdlib.String.t
  = "caml_jsstring_of_string"
(** Transcode a string from UTF-8 to UTF-16 for usage with JavaScript.

    This is an alias for {!val:utf16}. *)

(** {1 Math} *)

module Math : sig
  (** {2 Static properties} *)

  val e : number
  val ln10 : number
  val ln2 : number
  val log10e : number
  val log2e : number
  val pi : number
  val sqrt1_2 : number
  val sqrt2 : number

  (** {2 Static methods} *)

  val abs : number -> number
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Math/abs} [abs] on
        MDN}. *)

  val acos : number -> number
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Math/acos} [acos] on
        MDN}. *)

  val acosh : number -> number
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Math/acosh} [acosh] on
        MDN}. *)

  val asin : number -> number
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Math/asin} [asin] on
        MDN}. *)

  val asinh : number -> number
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Math/asinh} [asinh] on
        MDN}. *)

  val atan : number -> number
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Math/atan} [atan] on
        MDN}. *)

  val atanh : number -> number
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Math/atanh} [atanh] on
        MDN}. *)

  val atan2 : number -> number -> number
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Math/atan2} [atan2] on
        MDN}. *)

  val cbrt : number -> number
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Math/cbrt} [cbrt] on
        MDN}. *)

  val ceil : number -> number
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Math/ceil} [ceil] on
        MDN}. *)

  val clz32 : number -> number
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Math/clz32} [clz32] on
        MDN}. *)

  val cos : number -> number
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Math/cos} [cos] on
        MDN}. *)

  val cosh : number -> number
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Math/cosh} [cosh] on
        MDN}. *)

  val exp : number -> number
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Math/exp} [exp] on
        MDN}. *)

  val expm1 : number -> number
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Math/expm1} [expm1] on
        MDN}. *)

  val floor : number -> number
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Math/floor} [floor] on
        MDN}. *)

  val fround : number -> number
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Math/fround} [fround]
        on MDN}. *)

  val hypot : number Stdlib.Array.t -> number
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Math/hypot} [hypot] on
        MDN}. *)

  val imul : number -> number -> number
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Math/imul} [imul] on
        MDN}. *)

  val log : number -> number
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Math/log} [log] on
        MDN}. *)

  val log1p : number -> number
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Math/log1p} [log1p] on
        MDN}. *)

  val log10 : number -> number
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Math/log10} [log10] on
        MDN}. *)

  val log2 : number -> number
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Math/log2} [log2] on
        MDN}. *)

  val max : number Stdlib.Array.t -> number
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Math/max} [max] on
        MDN}. *)

  val min : number Stdlib.Array.t -> number
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Math/min} [min] on
        MDN}. *)

  val pow : number -> number -> number
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Math/pow} [pow] on
        MDN}. *)

  val random : unit -> number
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Math/random} [random]
        on MDN}. *)

  val round : number -> number
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Math/round} [round] on
        MDN}. *)

  val sign : number -> number
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Math/sign} [sign] on
        MDN}. *)

  val sin : number -> number
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Math/sin} [sin] on
        MDN}. *)

  val sinh : number -> number
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Math/sinh} [sinh] on
        MDN}. *)

  val sqrt : number -> number
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Math/sqrt} [sqrt] on
        MDN}. *)

  val tan : number -> number
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Math/tan} [tan] on
        MDN}. *)

  val tanh : number -> number
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Math/tanh} [tanh] on
        MDN}. *)

  val trunc : number -> number
  (** See
      {{:https://developer.mozilla.org/en-US/docs/Web/API/Math/trunc} [trunc] on
        MDN}. *)
end

(** {1 Global} *)

val global : any
(** See
    {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/globalThis}
      globalThis}.

    {[
      let window : [ `Window ] Jx.obj = Jx.get Jx.global "window"
    ]} *)

(* {b Note:} Library authors who want to publish their bindings should consider
   using the {{!module:Jx_ffi} jx.ffi} library which provides a minimal and
   dependency-free subset of the present FFI API. *)

(** {1:bindings Bindings}

    Bindings for accessing JavaScript definitions can be directly written in
    OCaml using the value encoding and decoding primitives provided below.

    The following example defines bindings for the
    {{:https://developer.mozilla.org/en-US/docs/Web/API/Document/getElementById}
      getElementById} method of the global
    {{:https://developer.mozilla.org/en-US/docs/Web/API/Document} document}
    object.

    {[
      module E = Jx.Encode
      module D = Jx.Decode

      type document = [ `Document ] Jx.obj
      type element = [ `Element ] Jx.obj

      (* Bind the global document object. *)
      let document : document = Jx.expr "document"

      (* Bind the getElementById method call. *)
      let get_element_by_id id : element =
        D.obj (D.meth document "getElementById" [| E.string id |])

      let () = Jx.log (get_element_by_id "container")
    ]}

    Which generates the following code (in addition to the OCaml runtime):

    {@javascript[
      log (document.getElementById "container")
    ]}

    In this example, the {!module:Decode} and {!module:Encode} modules are used
    to encode the method arguments and decode the return object. Type
    annotations are used to constrain the {{!type:obj} object class}.

    {b Warning:} The conversion modules and other low-level primitives provided
    here are unsafe by nature. When values are decoded or encoded, no static or
    dynamic type-checking is performed. Binding authors should ensure that
    correct type conversions and coercions are used. *)

(** {2:encode Encode}

    Encode OCaml values into opaque JavaScript values. *)

module Encode : sig
  val unit : unit -> any
  (** Encode an OCaml unit value as a JavaScript [undefined] value. *)

  external bool : bool -> any = "caml_js_from_bool"
  (** Encode an OCaml bool value. *)

  external int : int -> any = "%identity"
  (** Encode an OCaml int value. *)

  external char : char -> any = "%identity"
  (** Encode an OCaml char value as a JavaScript number. *)

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

  val optional : ('a -> any) -> 'a option -> any
  (** Encode an OCaml option as JavaScript value that can be [undefined]. *)

  val obj_optional : 'a obj option -> any
  (** Like {!val:undefined} but specialized for options of JavaScript objects. *)

  val any_optional : 'a option -> any
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

(** {2:decode Decode}

    Decode OCaml values from opaque JavaScript values. *)

module Decode : sig
  val unit : any -> unit
  (** Decode an OCaml unit value. *)

  external bool : any -> bool = "caml_js_to_bool"
  (** Decode an OCaml bool value. *)

  external int : any -> int = "%identity"
  (** Decode an OCaml int value. *)

  external char : any -> char = "%identity"
  (** Decode an OCaml char value from a JavaScript number. *)

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

  val optional : (any -> 'a) -> any -> 'a option
  (** Decode an OCaml option from a JavaScript value that can be [undefined]. *)

  val obj_optional : any -> 'a obj option
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

(** {2:raw Raw JavaScript}

    The {!expr} and {!stmt} primitives embed untyped JavaScript code into the
    compiled output.

    The textual representation of the code must be valid JavaScript, otherwise
    the compilation will fail.

    The provided JavaScript code must be represented as a static string literal.
    If the provided code string is computed dynamically, the evaluation will
    fallback to runtime and an error will be thrown (check the console for
    evaluation errors).

    {b Warning:} {!expr} and {!stmt} are unsafe since no type-checking is
    performed on the embedded code. *)

external expr : Stdlib.String.t -> 'c obj = "caml_pure_js_expr"
(** Unsafe JavaScript expression.

    {[
      assert (Jx.Decode.int (Jx.expr "2 + 2") = 4)
    ]} *)

external stmt : Stdlib.String.t -> unit = "caml_js_expr"
(** Unsafe JavaScript statement.

    {[
      let () = Jx.stmt "console.log('hello')"
    ]} *)

(** {1 Debug} *)

val debug : 'a -> unit
(** Print the runtime representation of a value using
    {{:https://developer.mozilla.org/en-US/docs/Web/API/console/debug_static}
      console.debug}.*)

val log : 'a -> unit
(** Print the runtime representation of a value using
    {{:https://developer.mozilla.org/en-US/docs/Web/API/console/log_static}
      console.log}.*)

external debugger : unit -> unit = "debugger"
(** See
    {{:https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/debugger}
      debugger}. *)
