type any

external expr : string -> any = "caml_pure_js_expr"
external exec : string -> unit = "caml_js_expr"
external magic : any -> 'a = "%identity"
external get : any -> string -> any = "caml_js_get"
external set : any -> string -> any -> unit = "caml_js_set"
external del : any -> string -> unit = "caml_js_delete"
external new_obj : any -> any array -> any = "caml_js_new"
external equal : any -> any -> bool = "caml_js_equals"
external strict_equal : any -> any -> bool = "caml_js_strict_equals"
external typeof : any -> string = "caml_js_typeof"
external instanceof : any -> constr:any -> bool = "caml_js_instanceof"
external call : any -> any array -> any = "caml_js_fun_call"
external meth : any -> string -> any array -> any = "caml_js_meth_call"
external debugger : unit -> unit = "debugger"

(** {JavaScript values} *)

external any : 'a -> any = "%identity"
external bool : bool -> any = "caml_js_from_bool"
external char : char -> any = "%identity"
external int : int -> any = "%identity"
external int32 : int32 -> any = "caml_js_from_int32"
external nativeint : nativeint -> any = "caml_js_from_nativeint"
external float : float -> any = "caml_js_from_float"
external string : string -> any = "%identity"
external ascii : string -> any = "%identity"
external array : 'a array -> any = "caml_js_from_array"
external list : 'a list -> any = "caml_list_to_js_array"
external obj : (string * 'a) array -> any = "caml_js_object"
external func : int -> (_ -> _) -> any = "caml_js_wrap_callback_strict"

(** {OCaml values} *)

module Ml = struct
  external array : any -> 'a array = "caml_js_to_array"
  external bool : any -> bool = "caml_js_to_bool"
  external char : any -> char = "%identity"
  external float : any -> float = "caml_js_to_float"
  external int : any -> int = "%identity"
  external list : any -> 'a list = "caml_list_of_js_array"
  external ascii : any -> string = "%identity"
  external string : any -> string = "%identity"
  external unicode : any -> string = "caml_string_of_jsstring"
end

(* {1 Unicode} *)

external unicode : string -> string = "caml_jsstring_of_string"
external utf16 : string -> string = "caml_jsstring_of_string"
external utf8 : string -> string = "caml_string_of_jsstring"

(* {1 Expressions} *)

let null : any = expr "null"
let undefined : any = expr "undefined"
let true' : any = expr "true"
let false' : any = expr "false"
let global_this : any = expr "globalThis"
let global prop : any = get global_this prop
let console : any = expr "console"

let log a =
  let _ = meth console "log" [| a |] in
  ()
