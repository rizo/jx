type t

external expr : string -> t = "caml_pure_js_expr"
external exec : string -> unit = "caml_js_expr"
external any : 'a -> t = "%identity"
external magic : t -> 'a = "%identity"
external get : t -> string -> t = "caml_js_get"
external set : t -> string -> t -> unit = "caml_js_set"
external del : t -> string -> unit = "caml_js_delete"
external new_obj : t -> t array -> t = "caml_js_new"
external equal : t -> t -> bool = "caml_js_equals"
external strict_equal : t -> t -> bool = "caml_js_strict_equals"
external typeof : t -> string = "caml_js_typeof"
external instanceof : t -> constr:t -> bool = "caml_js_instanceof"
external obj : (string * 'a) array -> t = "caml_js_object"
external call : t -> t array -> t = "caml_js_fun_call"
external meth : t -> string -> t array -> t = "caml_js_meth_call"
external debugger : unit -> unit = "debugger"

(** {JavaScript values} *)

module Encode = struct
  external bool : bool -> t = "caml_js_from_bool"
  external char : char -> t = "%identity"
  external int : int -> t = "%identity"
  external int32 : int32 -> t = "caml_js_from_int32"
  external nativeint : nativeint -> t = "caml_js_from_nativeint"
  external float : float -> t = "caml_js_from_float"
  external string : string -> t = "%identity"
  external ascii : string -> t = "%identity"
  external array : 'a array -> t = "caml_js_from_array"
  external list : 'a list -> t = "caml_list_to_js_array"
  external func : int -> (_ -> _) -> t = "caml_js_wrap_callback_strict"
end

(** {OCaml values} *)

module Decode = struct
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

(* {1 Unicode} *)

external unicode : string -> string = "caml_jsstring_of_string"
external utf16 : string -> string = "caml_jsstring_of_string"
external utf8 : string -> string = "caml_string_of_jsstring"

(* {1 Expressions} *)

let null : t = expr "null"
let undefined : t = expr "undefined"
let true' : t = expr "true"
let false' : t = expr "false"
let global_this : t = expr "globalThis"
let global prop : t = get global_this prop
let console : t = expr "console"

let log a =
  let _ = meth console "log" [| a |] in
  ()
