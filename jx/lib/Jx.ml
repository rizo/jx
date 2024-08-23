open struct
  let phys_equal = Stdlib.( == )
end

type any
type prop = Stdlib.String.t
type +'cls obj = any constraint 'cls = [> ]
type boolean = [ `Boolean ] obj
type number = [ `Number ] obj
type symbol = [ `Symbol ] obj
type function' = [ `Function ] obj
type 'a dict = [ `Dict of 'a ] obj
type 'a promise = [ `Promise of 'a ] obj
type +'a array = [ `Array of 'a ] obj
type +'a nullable = [ `Nullable of 'a ] obj
type +'a undefined = [ `Undefined of 'a ] obj

external expr : Stdlib.String.t -> 'c obj = "caml_pure_js_expr"
external stmt : Stdlib.String.t -> unit = "caml_js_expr"
external any : 'c obj -> any = "%identity"
external bool : Stdlib.Bool.t -> boolean = "caml_js_from_bool"
external to_bool : boolean -> Stdlib.Bool.t = "caml_js_to_bool"
external int : Stdlib.Int.t -> number = "%identity"
external to_int : number -> Stdlib.Int.t = "%identity"
external float : Stdlib.Float.t -> number = "caml_js_from_float"
external to_float : number -> Stdlib.Float.t = "caml_js_to_float"
external int32 : Int32.t -> number = "caml_js_from_int32"
external to_int32 : number -> Int32.t = "caml_js_to_int32"
external nativeint : nativeint -> number = "caml_js_from_nativeint"
external to_nativeint : number -> nativeint = "caml_js_to_nativeint"
external unicode : Stdlib.String.t -> string = "caml_jsstring_of_string"

module String = struct
  type t = [ `String ] obj
end

external string : Stdlib.String.t -> String.t = "%identity"
external to_string : String.t -> Stdlib.String.t = "%identity"
external utf16 : Stdlib.String.t -> Stdlib.String.t = "caml_jsstring_of_string"
external utf8 : Stdlib.String.t -> Stdlib.String.t = "caml_string_of_jsstring"

(* Array *)
external array : 'a Stdlib.Array.t -> 'a array = "caml_js_from_array"
external to_array : 'a array -> 'a Stdlib.Array.t = "caml_js_to_array"
external list : 'a list -> 'a array = "caml_list_to_js_array"
external to_list : 'a array -> 'a list = "caml_list_of_js_array"

let true' = expr "true"
let false' = expr "false"

(* external obj : any -> 'c obj = "%identity" *)

external any_array : 'a obj Stdlib.Array.t -> any Stdlib.Array.t = "%identity"

(* Type information *)

external typeof : 'c obj -> Stdlib.String.t = "caml_js_typeof"

external instanceof : 'c obj -> constr:'constr obj -> bool
  = "caml_js_instanceof"

(* Object *)

external get : 'c obj -> Stdlib.String.t -> any = "caml_js_get"
external set : 'c obj -> Stdlib.String.t -> 'v obj -> unit = "caml_js_set"
external del : 'c obj -> Stdlib.String.t -> unit = "caml_js_delete"

external obj : (Stdlib.String.t * any) Stdlib.Array.t -> 'a obj
  = "caml_js_object"

external obj_new : 'c obj -> any Stdlib.Array.t -> 'a obj = "caml_js_new"

(* Equality *)

external equal : 'c obj -> 'c obj -> bool = "caml_js_equals"
external strict_equal : 'c obj -> 'c obj -> bool = "caml_js_strict_equals"

(* debug *)

external debugger : unit -> unit = "debugger"

(* nullable *)

let null = expr "null"

external nullable : 'a -> 'a nullable = "%identity"

let is_null any = phys_equal any null

(* let nullable_of_any any = if is_null any then null else nullable any

   external any_of_nullable : 'a nullable -> any = "%identity" *)

module Nullable = struct
  type +'a t = 'a nullable

  external unsafe_get : 'a nullable -> 'a = "%identity"

  let of_any x_of_any any =
    if is_null any then null else nullable (x_of_any any)

  let to_any any_of_x this =
    if is_null this then this else any_of_x (unsafe_get this)

  let of_option opt =
    match opt with
    | None -> null
    | Some this -> nullable this

  let to_option this = if is_null this then None else Some (unsafe_get this)

  let get this =
    if is_null this then failwith "Nullable.get" else unsafe_get this

  let map f this = if is_null this then this else nullable (f (unsafe_get this))

  let map_or default f this =
    if is_null this then default else f (unsafe_get this)

  let map_or_else get_default f this =
    if is_null this then get_default () else f (unsafe_get this)
end

(* undefined *)

let undefined = expr "undefined"

external defined : 'a -> 'a undefined = "%identity"

let is_undefined any = phys_equal any undefined
let is_defined any = not (is_undefined any)

module Undefined = struct
  type +'a t = 'a undefined

  external unsafe_get : 'a undefined -> 'a = "%identity"

  let of_any x_of_any any =
    if is_undefined any then undefined else defined (x_of_any any)

  let to_any any_of_x this =
    if is_undefined this then this else any_of_x (unsafe_get this)

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

  let map_or default f this =
    if is_null this then default else f (unsafe_get this)

  let map_or_else get_default f this =
    if is_null this then get_default () else f (unsafe_get this)
end

(* Encode *)

module Encode = struct
  (* any, obj *)
  external any : 'a -> any = "%identity"
  external obj : 'a obj -> any = "%identity"

  (* unit *)
  let unit () = undefined

  (* bool, int, char, float *)
  external bool : bool -> any = "caml_js_from_bool"
  external int : int -> any = "%identity"
  external char : char -> any = "%identity"
  external float : float -> any = "caml_js_from_float"

  (* string *)
  external unicode : Stdlib.String.t -> any = "caml_jsstring_of_string"
  external string : Stdlib.String.t -> any = "%identity"

  (* array *)

  external obj_array : 'a obj Stdlib.Array.t -> any = "caml_js_from_array"
  external any_array : 'a Stdlib.Array.t -> any = "caml_js_from_array"

  let array ml_to_any ml_arr = obj_array (Array.map ml_to_any ml_arr)

  (* list *)

  external obj_list : any Stdlib.List.t -> any = "caml_list_to_js_array"
  external any_list : 'a Stdlib.List.t -> any = "caml_list_to_js_array"

  let list ml_to_any ml_lst = obj_list (List.map ml_to_any ml_lst)

  (* nullable *)

  external any_nullable : 'a option -> any = "%identity"

  let obj_nullable opt =
    match opt with
    | None -> null
    | Some x -> obj x

  let nullable encode opt =
    match opt with
    | None -> null
    | Some x -> encode x

  (* undefined *)

  external any_undefined : 'a option -> any = "%identity"

  let obj_undefined opt =
    match opt with
    | None -> undefined
    | Some x -> obj x

  let undefined encode opt =
    match opt with
    | None -> undefined
    | Some x -> encode x

  (* field *)

  let field obj prop encode x = set obj prop (encode x)

  (* func *)

  external func : int -> (_ -> _) -> any = "caml_js_wrap_callback_strict"

  (* dict *)

  external obj_dict : (Stdlib.String.t * 'a obj) Stdlib.Array.t -> any
    = "caml_js_object"

  external any_dict : (Stdlib.String.t * 'a) Stdlib.Array.t -> any
    = "caml_js_object"

  let dict ml_to_any fields =
    obj_dict (Stdlib.Array.map (fun (prop, v) -> (prop, ml_to_any v)) fields)

  (* let field obj prop ml_to_any ml_val = set obj prop (ml_to_any ml_val) *)

  module Array = struct
    external any : 'a Stdlib.Array.t -> any Stdlib.Array.t = "%identity"
    external obj : 'a obj Stdlib.Array.t -> any Stdlib.Array.t = "%identity"
  end
end

module Decode = struct
  (* obj *)
  external obj : any -> 'a obj = "%identity"

  (* unit *)
  let unit = ignore

  (* bool, int, float *)

  external bool : any -> bool = "caml_js_to_bool"
  external int : any -> int = "%identity"
  external float : any -> float = "caml_js_to_float"

  (* string *)

  external unicode : any -> Stdlib.String.t = "caml_string_of_jsstring"
  external string : any -> Stdlib.String.t = "%identity"

  (* array *)

  external obj_array : any -> 'a obj Stdlib.Array.t = "caml_js_to_array"

  let array any_to_ml any = Stdlib.Array.map any_to_ml (obj_array any)

  (* list *)

  external obj_list : any -> 'a obj Stdlib.List.t = "caml_list_of_js_array"

  let list any_to_ml any = Stdlib.List.map any_to_ml (obj_list any)

  (* nullable *)

  let nullable decode any = if is_null any then None else Some (decode any)
  let obj_nullable any = if is_null any then None else Some (obj any)

  (* undefined *)

  let undefined decode js = if is_undefined js then None else Some (decode js)
  let obj_undefined any = if is_undefined any then None else Some (obj any)

  (* func *)

  external func : any -> any Stdlib.Array.t -> any = "caml_js_fun_call"

  let field obj prop any_to_ml = any_to_ml (get obj prop)

  external meth : 'c obj -> Stdlib.String.t -> any Stdlib.Array.t -> 'ret obj
    = "caml_js_meth_call"
end

(* global *)

let global = expr "globalThis"

(* debug *)

let debug x =
  let _ = Decode.meth (expr "console") "debug" [| (Obj.magic x : any) |] in
  ()

let log x =
  let _ = Decode.meth (expr "console") "log" [| (Obj.magic x : any) |] in
  ()
