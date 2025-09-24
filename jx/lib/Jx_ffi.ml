(** A minimal and dependency-free subset of {{:../Jx/index.html} Jx}.

    This library is reserved for library authors. *)

type +'cls obj constraint 'cls = [> ]
type any = [ `Any ] obj
type prop = Stdlib.String.t

external expr : Stdlib.String.t -> 'c obj = "caml_pure_js_expr"
external exec : Stdlib.String.t -> unit = "caml_js_expr"
external any : 'c obj -> any = "%identity"

(** {2 Unicode} *)

external unicode : Stdlib.String.t -> string = "caml_jsstring_of_string"
external utf16 : Stdlib.String.t -> Stdlib.String.t = "caml_jsstring_of_string"
external utf8 : Stdlib.String.t -> Stdlib.String.t = "caml_string_of_jsstring"

(** {2 Object} *)

external get : 'c obj -> Stdlib.String.t -> 'v obj = "caml_js_get"
external set : 'c obj -> Stdlib.String.t -> 'v obj -> unit = "caml_js_set"
external del : 'c obj -> Stdlib.String.t -> unit = "caml_js_delete"

external obj : (Stdlib.String.t * any) Stdlib.Array.t -> 'a obj
  = "caml_js_object"

external obj_new : 'c obj -> any Stdlib.Array.t -> 'a obj = "caml_js_new"

(* Equality *)

external equal : 'c obj -> 'c obj -> bool = "caml_js_equals"
external strict_equal : 'c obj -> 'c obj -> bool = "caml_js_strict_equals"

(* Type information *)

external typeof : 'c obj -> Stdlib.String.t = "caml_js_typeof"

external instanceof : 'c obj -> constr:'constr obj -> bool
  = "caml_js_instanceof"

(** {2 Nullable} *)

module Nullable = struct
  type +'a t = [ `Nullable of 'a ] obj

  let null = expr "null"

  external make : 'a -> 'a t = "%identity"

  let is_null this = strict_equal this null

  external unsafe_get : 'a t -> 'a = "%identity"

  let of_any x_of_any any = if is_null any then null else make (x_of_any any)

  let to_any any_of_x this =
    if is_null this then any this else any_of_x (unsafe_get this)

  let of_option opt =
    match opt with
    | None -> null
    | Some this -> make this

  let to_option this = if is_null this then None else Some (unsafe_get this)

  let get this =
    if is_null this then failwith "Nullable.get" else unsafe_get this

  let map f this = if is_null this then null else make (f (unsafe_get this))

  let map_or default f this =
    if is_null this then default else f (unsafe_get this)

  let map_or_else get_default f this =
    if is_null this then get_default () else f (unsafe_get this)
end

type +'a nullable = 'a Nullable.t

external nullable : 'a -> 'a Nullable.t = "%identity"

let null = Nullable.null
let is_null = Nullable.is_null

(** {2 Optional} *)

module Optional = struct
  type +'a t = [ `Optional of 'a ] obj

  let undefined = expr "undefined"

  external defined : 'a -> 'a t = "%identity"

  let is_undefined this = strict_equal this undefined
  let is_defined this = not (is_undefined this)

  external unsafe_get : 'a t -> 'a = "%identity"

  let of_any x_of_any any =
    if is_undefined any then undefined else defined (x_of_any any)

  let to_any any_of_x this =
    if is_undefined this then any this else any_of_x (unsafe_get this)

  let of_option opt =
    match opt with
    | None -> undefined
    | Some this -> defined this

  let to_option this =
    if is_undefined this then None else Some (unsafe_get this)

  let get this =
    if is_undefined this then failwith "Optional.get" else unsafe_get this

  let map f this =
    if is_undefined this then undefined else defined (f (unsafe_get this))

  let map_or default f this =
    if is_undefined this then default else f (unsafe_get this)

  let map_or_else get_default f this =
    if is_undefined this then get_default () else f (unsafe_get this)
end

type +'a optional = 'a Optional.t

let undefined = Optional.undefined

external defined : 'a -> 'a Optional.t = "%identity"

let is_undefined = Optional.is_undefined
let is_defined = Optional.is_defined

(** {2 Conversion} *)

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
  external ascii : Stdlib.String.t -> any = "%identity"
  external string : Stdlib.String.t -> any = "%identity"

  (* array *)

  external obj_array : 'a obj Stdlib.Array.t -> any = "caml_js_from_array"
  external any_array : 'a Stdlib.Array.t -> any = "caml_js_from_array"

  let array ml_to_any ml_arr = obj_array (Array.map ml_to_any ml_arr)

  (* list *)

  external obj_list : 'a obj Stdlib.List.t -> any = "caml_list_to_js_array"
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

  (* optional *)

  external any_optional : 'a option -> any = "%identity"

  let obj_optional opt =
    match opt with
    | None -> undefined
    | Some x -> obj x

  let optional encode opt =
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
  external char : any -> char = "%identity"
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

  let optional decode js = if is_undefined js then None else Some (decode js)

  let obj_optional any =
    let any_obj = obj any in
    if is_undefined any_obj then None else Some any_obj

  (* func *)

  external func : any -> any Stdlib.Array.t -> any = "caml_js_fun_call"

  let field obj prop any_to_ml = any_to_ml (get obj prop)

  external meth : 'c obj -> Stdlib.String.t -> any Stdlib.Array.t -> 'ret obj
    = "caml_js_meth_call"
end

(** {2 Debug} *)

let debug x =
  ignore (Decode.meth (expr "console") "debug" [| (Obj.magic x : any) |])

let log x =
  ignore (Decode.meth (expr "console") "log" [| (Obj.magic x : any) |])

(* Debug *)

external debugger : unit -> unit = "debugger"

(** {2 Global} *)

let global : any = expr "globalThis"

(** {2 JavaScript types} *)

(* Boolean *)

type boolean = [ `Boolean ] obj

let true' : boolean = expr "true"
let false' : boolean = expr "false"

external bool : Stdlib.Bool.t -> boolean = "caml_js_from_bool"

(* Number *)

type number = [ `Number ] obj

external int : Stdlib.Int.t -> number = "%identity"
external float : Stdlib.Float.t -> number = "caml_js_from_float"
external int32 : Int32.t -> number = "caml_js_from_int32"
external nativeint : nativeint -> number = "caml_js_from_nativeint"

(* String *)

type string = [ `String ] obj

external string : Stdlib.String.t -> string = "%identity"

(* Symbol *)

type symbol = [ `Symbol ] obj

let symbol str : symbol =
  Decode.obj (Decode.func (expr "Symbol") [| Encode.string str |])

(* Array *)

type +'a array = [ `Array of 'a ] obj

(* external array : 'a Stdlib.Array.t -> 'a array = "caml_js_from_array" *)

let array values = obj_new (expr "Array") (Encode.Array.any values)

external list : 'a Stdlib.List.t -> 'a array = "caml_list_to_js_array"

(* Function *)

type function' = [ `Function ] obj

(* Dict *)

type 'a dict = [ `Dict of 'a ] obj

external dict : (prop * 'a) Stdlib.Array.t -> 'a dict = "caml_js_object"

(* Promise *)

type 'a promise = [ `Promise of 'a ] obj

(* BigInt *)

type bigint = [ `Bigint ] obj

(* Function *)

type func = [ `Func ] obj

(* Iterator *)

type 'a iterator = [ `Iterator of 'a ] obj
