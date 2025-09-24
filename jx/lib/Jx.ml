open struct
  module E = Jx_ffi.Encode
  module D = Jx_ffi.Decode
end

include Jx_ffi

(* Object *)
module Object = struct
  type 'a t = 'a Jx_ffi.obj
end

(* Boolean *)

module Boolean = struct
  type t = [ `Boolean ] Jx_ffi.obj

  let true' = Jx_ffi.expr "true"
  let false' = Jx_ffi.expr "false"

  external of_bool : Stdlib.Bool.t -> t = "caml_js_from_bool"
  external to_bool : t -> Stdlib.Bool.t = "caml_js_to_bool"

  let to_string this = D.string (D.meth this "toString" [||])
  let value_of this = D.bool (D.meth this "valueOf" [||])
end

(* Number *)

module Number = struct
  type t = [ `Number ] Jx_ffi.obj

  external of_int : Stdlib.Int.t -> t = "%identity"
  external to_int : t -> Stdlib.Int.t = "%identity"
  external of_float : Stdlib.Float.t -> t = "caml_js_from_float"
  external to_float : t -> Stdlib.Float.t = "caml_js_to_float"
  external of_int32 : Int32.t -> t = "caml_js_from_int32"
  external to_int32 : t -> Int32.t = "caml_js_to_int32"
  external of_nativeint : nativeint -> t = "caml_js_from_nativeint"
  external to_nativeint : t -> nativeint = "caml_js_to_nativeint"

  let is_finite num = D.bool (D.func (expr "Number.isFinite") [| E.obj num |])
  let is_integer num = D.bool (D.func (expr "Number.isInteger") [| E.obj num |])
  let is_nan num = D.bool (D.func (expr "Number.isNaN") [| E.obj num |])

  let is_safe_integer num =
    D.bool (D.func (expr "Number.isSafeInteger") [| E.obj num |])

  let parse_float str =
    D.obj (D.func (expr "Number.parseFloat") [| E.string str |])

  let parse_int str = D.obj (D.func (expr "Number.parseInt") [| E.string str |])

  let parse_int_with_radix ~radix str =
    D.obj (D.func (expr "Number.parseInt") [| E.string str; E.int radix |])

  let epsilon = expr "Number.EPSILON"
  let max_safe_integer = expr "Number.MAX_SAFE_INTEGER"
  let max_value = expr "Number.MAX_VALUE"
  let min_safe_integer = expr "Number.MIN_SAFE_INTEGER"
  let min_value = expr "Number.MIN_VALUE"
  let nan = expr "Number.NaN"
  let negative_infinity = expr "Number.NEGATIVE_INFINITY"
  let positive_infinity = expr "Number.POSITIVE_INFINITY"

  let to_exponential ~fraction_digits this =
    let fraction_digits = E.int fraction_digits in
    D.string (D.meth this "toExponential" [| fraction_digits |])

  let to_fixed ~fraction_digits this =
    let fraction_digits = E.int fraction_digits in
    D.string (D.meth this "toFixed" [| fraction_digits |])

  let to_locale_string ~locales this =
    let locales = E.obj locales in
    D.string (D.meth this "toLocaleString" [| locales |])

  let to_locale_string_with_options ~locales ~options this =
    let locales = E.obj locales in
    let options = E.obj options in
    D.string (D.meth this "toLocaleString" [| locales; options |])

  let to_precision ~precision this =
    let precision = E.int precision in
    D.string (D.meth this "toPrecision" [| precision |])

  let to_string ~radix this =
    let radix = E.int radix in
    D.string (D.meth this "toString" [| radix |])

  let value_of this = D.meth this "valueOf" [||]
end

(* String *)

module String = struct
  type t = [ `String ] Jx_ffi.obj

  external of_string : Stdlib.String.t -> t = "%identity"
  external to_string : t -> Stdlib.String.t = "%identity"
end

(* Symbol *)

module Symbol = struct
  type t = [ `Symbol ] Jx_ffi.obj

  let with_description = Jx_ffi.symbol
  let make () = D.obj (D.func (expr "Symbol") [||])
  let for_key key = D.obj (D.func (expr "Symbol.for") [| E.string key |])
  let key_for sym = D.string (D.func (expr "Symbol.keyFor") [| E.obj sym |])
  let has_instance = expr "Symbol.hasInstance"
  let is_concat_spreadable = expr "Symbol.isConcatSpreadable"
  let iterator = expr "Symbol.iterator"
  let match' = expr "Symbol.match"
  let match_all = expr "Symbol.matchAll"
  let replace = expr "Symbol.replace"
  let search = expr "Symbol.search"
  let species = expr "Symbol.species"
  let split = expr "Symbol.split"
  let to_primitive = expr "Symbol.toPrimitive"
  let to_string_tag = expr "Symbol.toStringTag"
  let unscopables = expr "Symbol.unscopables"
  let to_string sym = D.string (D.func (expr "Symbol.toString") [| E.obj sym |])
  let value_of this = D.meth this "valueOf" [||]
end

(* Array *)

module Array = struct
  type +'a t = [ `Array of 'a ] Jx_ffi.obj

  external of_array : 'a Stdlib.Array.t -> 'a t = "caml_js_from_array"
  external to_array : 'a t -> 'a Stdlib.Array.t = "caml_js_to_array"
  external of_list : 'a Stdlib.List.t -> 'a t = "caml_list_to_js_array"
  external to_list : 'a t -> 'a Stdlib.List.t = "caml_list_of_js_array"

  let empty () = expr "[]"
end

(* Dict *)

module Dict = struct
  type 'a t = [ `Dict of 'a ] Jx_ffi.obj

  external make : (Jx_ffi.prop * 'a) Stdlib.Array.t -> 'a t = "caml_js_object"
end

(* Promise *)

module Promise = struct
  type 'a t = [ `Promise of 'a ] Jx_ffi.obj
end

(* Math *)

module Math = struct
  let e = expr "Math.E"
  let ln10 = expr "Math.LN10"
  let ln2 = expr "Math.LN2"
  let log10e = expr "Math.LOG10E"
  let log2e = expr "Math.LOG2E"
  let pi = expr "Math.PI"
  let sqrt1_2 = expr "Math.SQRT1_2"
  let sqrt2 = expr "Math.SQRT2"

  let abs x =
    let x = E.obj x in
    D.obj (D.func (expr "Math.abs") [| x |])

  let acos x =
    let x = E.obj x in
    D.obj (D.func (expr "Math.acos") [| x |])

  let acosh x =
    let x = E.obj x in
    D.obj (D.func (expr "Math.acosh") [| x |])

  let asin x =
    let x = E.obj x in
    D.obj (D.func (expr "Math.asin") [| x |])

  let asinh x =
    let x = E.obj x in
    D.obj (D.func (expr "Math.asinh") [| x |])

  let atan x =
    let x = E.obj x in
    D.obj (D.func (expr "Math.atan") [| x |])

  let atanh x =
    let x = E.obj x in
    D.obj (D.func (expr "Math.atanh") [| x |])

  let atan2 x y =
    let x = E.obj x in
    let y = E.obj y in
    D.obj (D.func (expr "Math.atan2") [| x; y |])

  let cbrt x =
    let x = E.obj x in
    D.obj (D.func (expr "Math.cbrt") [| x |])

  let ceil x =
    let x = E.obj x in
    D.obj (D.func (expr "Math.ceil") [| x |])

  let clz32 x =
    let x = E.obj x in
    D.obj (D.func (expr "Math.clz32") [| x |])

  let cos x =
    let x = E.obj x in
    D.obj (D.func (expr "Math.cos") [| x |])

  let cosh x =
    let x = E.obj x in
    D.obj (D.func (expr "Math.cosh") [| x |])

  let exp x =
    let x = E.obj x in
    D.obj (D.func (expr "Math.exp") [| x |])

  let expm1 x =
    let x = E.obj x in
    D.obj (D.func (expr "Math.expm1") [| x |])

  let floor x =
    let x = E.obj x in
    D.obj (D.func (expr "Math.floor") [| x |])

  let fround x =
    let x = E.obj x in
    D.obj (D.func (expr "Math.fround") [| x |])

  let hypot values = D.obj (D.func (expr "Math.hypot") (E.Array.obj values))

  let imul x y =
    let x = E.obj x in
    let y = E.obj y in
    D.obj (D.func (expr "Math.imul") [| x; y |])

  let log x =
    let x = E.obj x in
    D.obj (D.func (expr "Math.log") [| x |])

  let log1p x =
    let x = E.obj x in
    D.obj (D.func (expr "Math.log1p") [| x |])

  let log10 x =
    let x = E.obj x in
    D.obj (D.func (expr "Math.log10") [| x |])

  let log2 x =
    let x = E.obj x in
    D.obj (D.func (expr "Math.log2") [| x |])

  let max values = D.obj (D.func (expr "Math.max") (E.Array.obj values))
  let min values = D.obj (D.func (expr "Math.min") (E.Array.obj values))

  let pow x y =
    let x = E.obj x in
    let y = E.obj y in
    D.obj (D.func (expr "Math.pow") [| x; y |])

  let random () = D.obj (D.func (expr "Math.random") [||])

  let round x =
    let x = E.obj x in
    D.obj (D.func (expr "Math.round") [| x |])

  let sign x =
    let x = E.obj x in
    D.obj (D.func (expr "Math.sign") [| x |])

  let sin x =
    let x = E.obj x in
    D.obj (D.func (expr "Math.sin") [| x |])

  let sinh x =
    let x = E.obj x in
    D.obj (D.func (expr "Math.sinh") [| x |])

  let sqrt x =
    let x = E.obj x in
    D.obj (D.func (expr "Math.sqrt") [| x |])

  let tan x =
    let x = E.obj x in
    D.obj (D.func (expr "Math.tan") [| x |])

  let tanh x =
    let x = E.obj x in
    D.obj (D.func (expr "Math.tanh") [| x |])

  let trunc x =
    let x = E.obj x in
    D.obj (D.func (expr "Math.trunc") [| x |])
end

module Func = struct
  type t = func

  let make args = obj_new (expr "Function") (E.Array.any args)
  let apply ~this ~args func = D.meth func "apply" [| E.obj this; E.obj args |]

  let bind ~this ~args func =
    D.obj (D.meth func "bind" [| E.obj this; E.obj args |])

  let call ~this ~args func = D.meth func "call" [| E.obj this; E.obj args |]
  let to_string this = D.string (D.meth this "toString" [||])
  let length this = D.int (D.meth this "length" [||])
  let name this = D.string (D.meth this "name" [||])
end

module Iterator = struct
  type 'a t = 'a iterator

  let make () = obj_new (expr "Iterator") [||]
  let from obj = D.obj (D.func (expr "Iterator.from") [| E.obj obj |])
  let drop limit this = D.obj (D.meth this "drop" [| E.int limit |])

  let every callback_fn this =
    let callback_fn = E.func 2 callback_fn in
    D.bool (D.meth this "every" [| callback_fn |])

  let every' callback_fn_any this =
    D.bool (D.meth this "every" [| callback_fn_any |])
end
