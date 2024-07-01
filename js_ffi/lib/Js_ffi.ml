let phys_equal = Stdlib.( == )

type any
type +'cls obj = any constraint 'cls = [> ]

(* unsafe *)

external raw : string -> any = "caml_pure_js_expr"

(* nullable *)

type +'a nullable = any

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

type +'a undefined = any

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

external get : any -> string -> any = "caml_js_get"
external set : any -> string -> any -> unit = "caml_js_set"
external del : any -> string -> unit = "caml_js_delete"
external obj : (string * any) array -> 'a obj = "caml_js_object"
external obj_new : any -> any array -> 'a obj = "caml_js_new"

(* Encode *)

module Encode = struct
  external js : any -> any = "%identity"
  external any : 'a -> any = "%identity"
  external obj : 'a obj -> any = "%identity"
  external int : int -> any = "%identity"
  external char : char -> any = "%identity"
  external unit : unit -> any = "%identity"
  external bool : bool -> any = "caml_js_from_bool"
  external float : float -> any = "caml_js_from_float"
  external string : string -> any = "caml_jsstring_of_string"
  external string_ascii : string -> any = "%identity"
  external js_array : any array -> any = "caml_js_from_array"

  let array encode arr = js_array (Array.map encode arr)

  external js_nullable : any nullable -> any = "%identity"
  external js_undefined : any undefined -> any = "%identity"

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

  external fun' : int -> (_ -> _) -> any = "caml_js_wrap_callback_strict"
end

module Decode = struct
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

external fun_call : any -> any array -> any = "caml_js_fun_call"
external meth_call : any -> string -> any array -> any = "caml_js_meth_call"

let global_this = raw "globalThis"
let global prop = get global_this prop

(* equal *)

external equal : any -> any -> bool = "caml_js_equals"
external strict_equal : any -> any -> bool = "caml_js_strict_equals"

(* type *)

external typeof : any -> string = "caml_js_typeof"
external instanceof : 'a obj -> constr:any -> bool = "caml_js_instanceof"

(* debug *)

external debugger : unit -> unit = "debugger"

let debug x = meth_call (raw "console") "debug" [| Obj.magic x |] |> ignore
