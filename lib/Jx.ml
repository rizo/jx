type t

external expr : string -> t = "caml_pure_js_expr"
external exec : string -> unit = "caml_js_expr"
external equal : t -> t -> bool = "caml_js_equals"
external strict_equal : t -> t -> bool = "caml_js_strict_equals"
external typeof : t -> string = "caml_js_typeof"
external instanceof : t -> constr:t -> bool = "caml_js_instanceof"
external obj_get : t -> string -> t = "caml_js_get"
external obj_set : t -> string -> t -> unit = "caml_js_set"
external obj_delete : t -> string -> unit = "caml_js_delete"
external obj_new : t -> t array -> t = "caml_js_new"
external obj_call : t -> string -> t array -> t = "caml_js_meth_call"
external arr_get : t -> int -> t = "caml_js_get"
external arr_set : t -> int -> t -> unit = "caml_js_set"
external arr_delete : t -> int -> unit = "caml_js_delete"
external call : t -> t array -> t = "caml_js_fun_call"
external debugger : unit -> unit = "debugger"

module Encode = struct
  external any : 'a -> t = "%identity"
  external bool : bool -> t = "caml_js_from_bool"
  external char : char -> t = "%identity"
  external int : int -> t = "%identity"
  external int32 : int32 -> t = "caml_js_from_int32"
  external nativeint : nativeint -> t = "caml_js_from_nativeint"
  external float : float -> t = "caml_js_from_float"
  external string : string -> t = "%identity"
  external ascii : string -> t = "%identity"
  external unicode : string -> string = "caml_jsstring_of_string"
  external obj : (string * 'a) array -> t = "caml_js_object"
  external array : 'a array -> t = "caml_js_from_array"
  external list : 'a list -> t = "caml_list_to_js_array"
  external func : int -> (_ -> _) -> t = "caml_js_wrap_callback_strict"
end

module Decode = struct
  external any : t -> 'a = "%identity"
  external array : t -> 'a array = "caml_js_to_array"
  external bool : t -> bool = "caml_js_to_bool"
  external char : t -> char = "%identity"
  external float : t -> float = "caml_js_to_float"
  external int : t -> int = "%identity"
  external list : t -> 'a list = "caml_list_of_js_array"
  external ascii : t -> string = "%identity"
  external string : t -> string = "%identity"
  external unicode : t -> string = "caml_string_of_jsstring"
end

(* {1 Expressions} *)

let null : t = expr "null"
let undefined : t = expr "undefined"
let global_this : t = expr "globalThis"
let global prop : t = obj_get global_this prop
let console : t = expr "console"

let log a =
  let _ = obj_call console "log" [| Encode.any a |] in
  ()

let debug a =
  let _ = obj_call console "debug" [| Encode.any a |] in
  ()
