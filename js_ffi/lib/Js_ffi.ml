let phys_equal = Stdlib.( == )

type t
type js = t
type +'cls obj = t constraint 'cls = [> ]

(* unsafe *)

external raw : string -> js = "caml_pure_js_expr"

(* nullable *)

type +'a nullable = js

let null = raw "null"
let nullable x = Obj.magic x
let is_null any = phys_equal any null

module Nullable = struct
  type +'a t = 'a nullable

  let unsafe_get = Obj.magic

  let of_option opt =
    match opt with
    | None -> null
    | Some this -> nullable this

  let to_option this = if is_null this then None else Some (unsafe_get this)

  let get this =
    if is_null this then failwith "Nullable.get" else unsafe_get this

  let map f this = if is_null this then this else nullable (f (unsafe_get this))
end

(* undefined *)

type +'a undefined = js

let undefined = raw "undefined"
let defined x = Obj.magic x
let is_undefined any = phys_equal any undefined
let is_defined any = not (is_undefined any)

module Undefined = struct
  type +'a t = 'a undefined

  let unsafe_get = Obj.magic

  let of_option opt =
    match opt with
    | None -> undefined
    | Some this -> defined this

  let to_option this =
    if is_undefined this then None else Some (unsafe_get this)

  let get this =
    if is_undefined this then failwith "Undefined.get" else unsafe_get this

  let map f this =
    if is_undefined this then this else defined (f (unsafe_get this))
end

(* obj *)

external get : js -> string -> js = "caml_js_get"
external set : js -> string -> js -> unit = "caml_js_set"
external del : js -> string -> unit = "caml_js_delete"
external obj : (string * js) array -> 'a obj = "caml_js_object"
external obj_new : js -> js array -> 'a obj = "caml_js_new"

(* Encode *)

module Encode = struct
  external js : js -> js = "%identity"
  external any : 'a -> js = "%identity"
  external obj : 'a obj -> js = "%identity"
  external int : int -> js = "%identity"
  external char : char -> js = "%identity"
  external unit : unit -> js = "%identity"
  external bool : bool -> js = "caml_js_from_bool"
  external float : float -> js = "caml_js_from_float"
  external string : string -> js = "caml_jsstring_of_string"
  external string_ascii : string -> js = "%identity"
  external js_array : js array -> js = "caml_js_from_array"

  let array encode arr = js_array (Array.map encode arr)

  external js_nullable : js nullable -> js = "%identity"
  external js_undefined : js undefined -> js = "%identity"

  let option_as_nullable encode opt =
    match opt with
    | None -> null
    | Some x -> encode x

  let option_as_undefined encode opt =
    match opt with
    | None -> undefined
    | Some x -> encode x

  let nullable encode nullable = Nullable.map encode nullable
  let undefined encode undefined = Nullable.map encode undefined

  external fun' : int -> (_ -> _) -> js = "caml_js_wrap_callback_strict"
end

module Decode = struct
  external js : js -> js = "%identity"
  external any : js -> 'a = "%identity"
  external obj : js -> 'a obj = "%identity"
  external int : js -> int = "%identity"
  external unit : js -> unit = "%identity"
  external bool : js -> bool = "caml_js_to_bool"
  external float : js -> float = "caml_js_to_float"
  external string : js -> string = "caml_string_of_jsstring"
  external string_ascii : js -> string = "%identity"
  external js_array : js -> js array = "caml_js_to_array"

  let array decode js = Array.map decode (js_array js)
  let nullable decode js = if is_null js then null else nullable (decode js)

  let undefined decode js =
    if is_null js then undefined else defined (decode js)

  let nullable_as_option decode js =
    if is_null js then None else Some (decode js)

  let undefined_as_option decode js =
    if is_undefined js then None else Some (decode js)
end

(* fun *)

external fun_call : js -> js array -> js = "caml_js_fun_call"
external meth_call : js -> string -> js array -> js = "caml_js_meth_call"

let global_this = raw "globalThis"
let global prop = get global_this prop

(* equal *)

external equal : js -> js -> bool = "caml_js_equals"
external strict_equal : js -> js -> bool = "caml_js_strict_equals"

(* type *)

external typeof : js -> string = "caml_js_typeof"
external instanceof : 'a obj -> constr:js -> bool = "caml_js_instanceof"

(* debug *)

external debugger : unit -> unit = "debugger"

let debug x = meth_call (raw "console") "debug" [| Obj.magic x |] |> ignore
