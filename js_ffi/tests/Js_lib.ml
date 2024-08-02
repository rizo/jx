module Jx = Js_ffi

module Date : sig
  type t = [ `Date ] Js_ffi.obj

  external of_any : Jx.any -> t = "%identity"
  external to_any : t -> Jx.any = "%identity"
  val make : unit -> t

  val make_with_value :
    value:[< `Number | `String | `Date ] Js_ffi.obj -> unit -> t

  val now : unit -> t
  val parse : string:Jx.string -> unit -> t
  val get_date : t -> Jx.number
  val set_date : date:Jx.number -> t -> Jx.number
end = struct
  type t = [ `Date ] Js_ffi.obj

  let t = Jx.expr "Date"

  external of_any : Jx.any -> t = "%identity"
  external to_any : t -> Jx.any = "%identity"

  let make () = Js_ffi.obj_new t [||]

  let make_with_value ~value () =
    let value = Jx.any value in
    Js_ffi.obj_new t [| value |]

  let now () = of_any (Js_ffi.meth t "now" [||])

  let parse ~string () =
    let string = Jx.any string in
    of_any (Js_ffi.meth t "parse" [| string |])

  let get_date this = Js_ffi.meth this "getDate" [||]

  let set_date ~date this =
    let date = Jx.any date in
    Js_ffi.meth this "setDate" [| date |]
end

module Event_init : sig
  type t = [ `Event_init ] Jx.obj

  external of_any : Jx.any -> t = "%identity"
  external to_any : t -> Jx.any = "%identity"
  val empty : unit -> t
  val set_bubbles : t -> Jx.boolean -> unit
  val set_cancelable : t -> Jx.boolean -> unit
  val set_composed : t -> Jx.boolean -> unit
  val bubbles : t -> Jx.boolean
  val cancelable : t -> Jx.boolean
  val composed : t -> Jx.boolean
end = struct
  type t = [ `Event_init ] Jx.obj

  external of_any : Jx.any -> t = "%identity"
  external to_any : t -> Jx.any = "%identity"

  let empty () = Jx.obj [||]
  let set_bubbles this bubbles = Jx.set this "bubbles" (Jx.any bubbles)

  let set_cancelable this cancelable =
    Jx.set this "cancelable" (Jx.any cancelable)

  let set_composed this composed = Jx.set this "composed" (Jx.any composed)
  let bubbles this = Jx.get this "bubbles"
  let cancelable this = Jx.get this "cancelable"
  let composed this = Jx.get this "composed"
end

module rec Dom_high_res_time_stamp : sig
  type nonrec t = float

  external of_any : Jx.any -> t = "caml_js_to_float"
  external to_any : t -> Jx.any = "caml_js_from_float"
end =
  Dom_high_res_time_stamp

and Event_target : sig
  type t = [ `Event_target ] Jx.obj

  external of_any : Jx.any -> t = "%identity"
  external to_any : t -> Jx.any = "%identity"
  val make : unit -> t

  val add_event_listener :
    type':Jx.string -> callback:Event_listener.t Jx.nullable -> t -> unit

  val add_event_listener_with_options :
    type':Jx.string ->
    callback:Event_listener.t Jx.nullable ->
    options:[< `Add_event_listener_options | `Bool ] Jx.obj ->
    t ->
    unit

  val remove_event_listener :
    type':Jx.string -> callback:Event_listener.t Jx.nullable -> t -> unit

  val remove_event_listener_with_options :
    type':Jx.string ->
    callback:Event_listener.t Jx.nullable ->
    options:[< `Event_listener_options | `Bool ] Jx.obj ->
    t ->
    unit

  val dispatch_event : event:Event.t -> t -> Jx.boolean
end = struct
  type t = [ `Event_target ] Jx.obj

  let t = Jx.expr "EventTarget"

  external of_any : Jx.any -> t = "%identity"
  external to_any : t -> Jx.any = "%identity"

  let make () = Jx.obj_new t [||]

  let add_event_listener ~type' ~callback this =
    let type' = Jx.any type' in
    let callback = Jx.any (Jx.Nullable.map Event_listener.to_any callback) in
    ignore (Jx.meth this "addEventListener" [| type'; callback |])

  let add_event_listener_with_options ~type' ~callback ~options this =
    let type' = Jx.any type' in
    let callback = Jx.any (Jx.Nullable.map Event_listener.to_any callback) in
    let options = Jx.any options in
    ignore (Jx.meth this "addEventListener" [| type'; callback; options |])

  let remove_event_listener ~type' ~callback this =
    let type' = Jx.any type' in
    let callback = Jx.any (Jx.Nullable.map Event_listener.to_any callback) in
    ignore (Jx.meth this "removeEventListener" [| type'; callback |])

  let remove_event_listener_with_options ~type' ~callback ~options this =
    let type' = Jx.any type' in
    let callback = Jx.any (Jx.Nullable.map Event_listener.to_any callback) in
    let options = Jx.any options in
    ignore (Jx.meth this "removeEventListener" [| type'; callback; options |])

  let dispatch_event ~event this =
    let event = Event.to_any event in
    Jx.meth this "dispatchEvent" [| event |]
end

and Event : sig
  type t = [ `Event ] Jx.obj

  external of_any : Jx.any -> t = "%identity"
  external to_any : t -> Jx.any = "%identity"
  val make : type':string -> unit -> t

  val with_type_and_event_init_dict :
    type':string -> event_init_dict:Event_init.t -> unit -> t

  val type' : t -> Jx.string
  val target : t -> Event_target.t Jx.nullable
  val src_element : t -> Event_target.t Jx.nullable
  val current_target : t -> Event_target.t Jx.nullable
  val composed_path : t -> Event_target.t Jx.array
  val none : Jx.number
  val capturing_phase : Jx.number
  val at_target : Jx.number
  val bubbling_phase : Jx.number
  val event_phase : t -> Jx.number
  val stop_propagation : t -> unit
  val cancel_bubble : t -> bool
  val set_cancel_bubble : t -> bool -> unit
  val stop_immediate_propagation : t -> unit
  val bubbles : t -> Jx.boolean
  val cancelable : t -> Jx.boolean
  val return_value : t -> Jx.boolean
  val set_return_value : t -> Jx.boolean -> unit
  val prevent_default : t -> unit
  val default_prevented : t -> Jx.boolean
  val composed : t -> Jx.boolean
  val is_trusted : t -> Jx.boolean
  val time_stamp : t -> Dom_high_res_time_stamp.t
  val init_event : type':string -> t -> unit
  val init_event_with_bubbles : type':string -> bubbles:bool -> t -> unit

  val init_event_with_bubbles_and_cancelable :
    type':string -> bubbles:bool -> cancelable:bool -> t -> unit
end = struct
  type t = [ `Event ] Jx.obj

  let t = Jx.expr "Event"

  external of_any : Jx.any -> t = "%identity"
  external to_any : t -> Jx.any = "%identity"

  let with_type_and_event_init_dict ~type' ?event_init_dict () =
    let type' = Jx.Any.of_string type' in
    let event_init_dict =
      (Jx.Any.undefined_of_option Event_init.to_any) event_init_dict
    in
    Jx.obj_new t [| type'; event_init_dict |]

  let type' this = Jx.Any.to_string (Jx.get this "type")

  let target this =
    (Jx.Any.nullable_to_option Event_target.of_any) (Jx.get this "target")

  let src_element this =
    (Jx.Any.nullable_to_option Event_target.of_any) (Jx.get this "srcElement")

  let current_target this =
    (Jx.Any.nullable_to_option Event_target.of_any)
      (Jx.get this "currentTarget")

  let composed_path this =
    (Jx.Any.to_array Event_target.of_any)
      (Jx.meth_call this "composedPath" [||])

  let none = Jx.int 0
  let capturing_phase = Jx.int 1
  let at_target = Jx.int 2
  let bubbling_phase = Jx.int 3
  let event_phase this = Jx.Any.to_int (Jx.get this "eventPhase")
  let stop_propagation this = ignore (Jx.meth_call this "stopPropagation" [||])
  let cancel_bubble this = Jx.Any.to_bool (Jx.get this "cancelBubble")
  let set_cancel_bubble this x = Jx.set this "cancelBubble" (Jx.Any.of_bool x)

  let stop_immediate_propagation this =
    ignore (Jx.meth_call this "stopImmediatePropagation" [||])

  let bubbles this = Jx.Any.to_bool (Jx.get this "bubbles")
  let cancelable this = Jx.Any.to_bool (Jx.get this "cancelable")
  let return_value this = Jx.Any.to_bool (Jx.get this "returnValue")
  let set_return_value this x = Jx.set this "returnValue" (Jx.Any.of_bool x)
  let prevent_default this = ignore (Jx.meth_call this "preventDefault" [||])
  let default_prevented this = Jx.Any.to_bool (Jx.get this "defaultPrevented")
  let composed this = Jx.Any.to_bool (Jx.get this "composed")
  let is_trusted this = Jx.Any.to_bool (Jx.get this "isTrusted")
  let time_stamp this = Dom_high_res_time_stamp.of_any (Jx.get this "timeStamp")

  let init_event ~type' ?bubbles ?cancelable this =
    let type' = Jx.Any.of_string type' in
    let bubbles = (Jx.Any.undefined_of_option Jx.Any.of_bool) bubbles in
    let cancelable = (Jx.Any.undefined_of_option Jx.Any.of_bool) cancelable in
    ignore (Jx.meth_call this "initEvent" [| type'; bubbles; cancelable |])
end

and Event_listener : sig
  type t = Event.t -> unit

  val of_any : Jx.any -> t
  val to_any : t -> Jx.any
end = struct
  type t = Event.t -> unit

  let to_any this = Jx.Any.of_fun 1 this

  let of_any any =
    let __f_js = Jx.magic any in
    fun event ->
      let event = Event.to_any event in
      Jx.to_unit (Jx.fun_call __f_js [| event |])
end

module Node : sig
  type t = [ `Node ] Jx.obj

  external of_any : Jx.any -> t = "%identity"
  external to_any : t -> Jx.any = "%identity"
  val append_child : node:t -> t -> t
end = struct
  type t = [ `Node ] Jx.obj

  external of_any : Jx.any -> t = "%identity"
  external to_any : t -> Jx.any = "%identity"

  let append_child ~node this =
    of_any (Jx.meth this "appendChild" [| to_any node |])
end

module Element : sig
  type t = [ `Element ] Jx.obj

  external to_node : t -> Node.t = "%identity"
  external of_any : Jx.any -> t = "%identity"
  external to_any : t -> Jx.any = "%identity"

  val append :
    nodes:[< `Node | `Trusted_script | `String ] Jx.obj array -> t -> unit
end = struct
  type t = [ `Element ] Jx.obj

  external to_node : t -> Node.t = "%identity"
  external of_any : Jx.any -> t = "%identity"
  external to_any : t -> Jx.any = "%identity"

  let append ~nodes this =
    ignore (Js_ffi.meth this "append" (Jx.any_array nodes))
end

module Text : sig
  type t = [ `Text ] Jx.obj

  external to_node : t -> Node.t = "%identity"
  external of_any : Jx.any -> t = "%identity"
  external to_any : t -> Jx.any = "%identity"
  val make : unit -> t
  val make_with_data : data:Jx.string -> unit -> t
end = struct
  type t = [ `Text ] Jx.obj

  let t = Jx.expr "Text"

  external to_node : t -> Node.t = "%identity"
  external of_any : Jx.any -> t = "%identity"
  external to_any : t -> Jx.any = "%identity"

  let make () = Jx.obj_new t [||]

  let make_with_data ~data () =
    let data = Jx.any data in
    Jx.obj_new t [| data |]
end

module Html_collection : sig
  type t = [ `Html_collection ] Jx.obj

  external of_any : Jx.any -> t = "%identity"
  external to_any : t -> Jx.any = "%identity"
  val length : t -> Jx.number
  val item : index:Jx.number -> t -> Element.t Jx.nullable
end = struct
  type t = [ `Html_collection ] Jx.obj

  let t = Jx.expr "HTMLCollection"

  external of_any : Jx.any -> t = "%identity"
  external to_any : t -> Jx.any = "%identity"

  let length this = Jx.get this "length"

  let item ~index this =
    let index = Jx.any index in
    Js_ffi.meth this "item" [| index |]
end

module Document : sig
  type t = [ `Document ] Jx.obj

  external to_node : t -> Node.t = "%identity"
  external of_any : Jx.any -> t = "%identity"
  external to_any : t -> Jx.any = "%identity"
  val make : unit -> t
  val create_element : tag_name:Jx.string -> t -> Element.t

  val create_element_with_options :
    tag_name:Jx.string ->
    options:[< `String | `Element_creation_options ] Jx.obj ->
    t ->
    Element.t

  val create_text_node : data:Jx.string -> t -> Text.t
  val query_selector : selectors:Jx.string -> t -> Element.t Jx.nullable
  val children : t -> Html_collection.t
  val import_node : node:Node.t -> t -> Node.t
  val import_node_with_deep : node:Node.t -> deep:Jx.boolean -> t -> Node.t
end = struct
  type t = [ `Document ] Jx.obj

  let t = Jx.expr "Document"
  let make () = Js_ffi.obj_new t [||]

  external to_node : t -> Node.t = "%identity"
  external of_any : Jx.any -> t = "%identity"
  external to_any : t -> Jx.any = "%identity"

  let create_element ~tag_name this =
    let tag_name = Jx.any tag_name in
    Element.of_any (Jx.meth this "createElement" [| tag_name |])

  let create_element_with_options ~tag_name ~options this =
    let tag_name = Jx.any tag_name in
    let options = Jx.any options in
    Element.of_any (Jx.meth this "createElement" [| tag_name; options |])

  let create_text_node ~data this =
    Text.of_any (Jx.meth this "createTextNode" [| Jx.any data |])

  let query_selector ~selectors this =
    Jx.meth this "querySelector" [| Jx.any selectors |]

  let children this = Html_collection.of_any (Jx.get this "children")

  let import_node ~node this =
    let node = Node.to_any node in
    Node.of_any (Jx.meth this "importNode" [| node |])

  let import_node_with_deep ~node ~deep this =
    let node = Node.to_any node in
    let deep = Jx.any deep in
    Node.of_any (Jx.meth this "importNode" [| node; deep |])
end

let window : [ `Window ] Jx.obj = Jx.get Jx.global "window"

let parse_int_js (str : Jx.string) (radix : Jx.number) : Jx.number =
  Jx.call (Jx.expr "parseInt") [| Jx.any str; Jx.any radix |]

let parse_int_ml (str : string) (radix : int) : int =
  Jx.call (Jx.expr "parseInt")
    [| Jx.any (Jx.ascii str); Jx.any (Jx.int radix) |]
  |> Jx.to_int

let parse_int_ml (str : string) (radix : int) : int =
  Jx.call2 (Jx.expr "parseInt") (Jx.ascii str, Jx.int radix) |> Jx.to_int

let alert str = ignore (Jx.call (Jx.expr "window.alert") [| Jx.any str |])
let document = Jx.expr "document" |> Document.of_any

let query_selector ~(selectors : Jx.string) (this : Document.t) : Element.t =
  Jx.meth this "querySelector" [| Jx.any selectors |]

let word_count str =
  let src =
    {|
      function countWords(str) {
        return str.trim().split(/\s+/).length;
      }
    |}
  in
  Jx.to_int (Jx.call (Jx.expr src) [| Jx.any (Jx.ascii str) |])
