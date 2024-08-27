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

  let make = Jx_ffi.symbol

  let empty () =
    Jx_ffi.Decode.obj (Jx_ffi.Decode.func (Jx_ffi.expr "Symbol") [||])
end

(* Array *)

module Array = struct
  type +'a t = [ `Array of 'a ] Jx_ffi.obj

  external of_array : 'a Stdlib.Array.t -> 'a t = "caml_js_from_array"
  external to_array : 'a t -> 'a Stdlib.Array.t = "caml_js_to_array"
  external of_list : 'a Stdlib.List.t -> 'a t = "caml_list_to_js_array"
  external to_list : 'a t -> 'a Stdlib.List.t = "caml_list_of_js_array"
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
