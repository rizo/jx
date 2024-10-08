module E = Jx.Encode
module D = Jx.Decode

type date = [ `Date ] Jx.obj
and event_init = [ `Event_init ] Jx.obj
and dom_high_res_time_stamp = float
and event_listener = event -> unit
and event = [ `Event ] Jx.obj
and event_target = [ `Event_target ] Jx.obj

external event_target_of_any : Jx.any -> event_target = "%identity"
external event_target_to_any : Jx.any -> event_target = "%identity"

external dom_high_res_time_stamp_of_any : Jx.any -> dom_high_res_time_stamp
  = "caml_js_to_float"

external dom_high_res_time_stamp_to_any : dom_high_res_time_stamp -> Jx.any
  = "caml_js_from_float"

module Date : sig
  type t = [ `Date ] Jx.obj

  external of_any : Jx.any -> t = "%identity"
  external to_any : t -> Jx.any = "%identity"
  val make : unit -> t
  val make_with_value : value:[< `Number | `String | `Date ] Jx.obj -> unit -> t
  val now : unit -> t
  val parse : string:string -> unit -> t
  val get_date : t -> int
  val set_date : date:int -> t -> int
end = struct
  type t = [ `Date ] Jx.obj

  let t = Jx.expr "Date"

  external of_any : Jx.any -> t = "%identity"
  external to_any : t -> Jx.any = "%identity"

  let make () = Jx.obj_new t [||]

  let make_with_value ~value () =
    let value = E.any value in
    Jx.obj_new t [| value |]

  let now () = of_any (D.meth t "now" [||])

  let parse ~string () =
    let string = E.string string in
    of_any (D.meth t "parse" [| string |])

  let get_date this = D.int (D.meth this "getDate" [||])

  let set_date ~date this =
    let date = E.int date in
    D.int (D.meth this "setDate" [| date |])
end

module Event_init : sig
  type t = [ `Event_init ] Jx.obj

  external of_any : Jx.any -> t = "%identity"
  external to_any : t -> Jx.any = "%identity"
  val empty : unit -> t
  val set_bubbles : t -> bool -> unit
  val set_cancelable : t -> bool -> unit
  val set_composed : t -> bool -> unit
  val bubbles : t -> bool
  val cancelable : t -> bool
  val composed : t -> bool
end = struct
  type t = [ `Event_init ] Jx.obj

  external of_any : Jx.any -> t = "%identity"
  external to_any : t -> Jx.any = "%identity"

  let empty () = Jx.obj [||]
  let set_bubbles this bubbles = E.field this "bubbles" E.bool bubbles

  let set_cancelable this cancelable =
    E.field this "cancelable" E.bool cancelable

  let set_composed this composed = E.field this "composed" E.bool composed
  let bubbles this = D.field this "bubbles" D.bool
  let cancelable this = D.field this "cancelable" D.bool
  let composed this = D.field this "composed" D.bool
end

module Dom_high_res_time_stamp : sig
  type nonrec t = float

  external of_any : Jx.any -> t = "caml_js_to_float"
  external to_any : t -> Jx.any = "caml_js_from_float"
end = struct
  type nonrec t = float

  external of_any : Jx.any -> t = "caml_js_to_float"
  external to_any : t -> Jx.any = "caml_js_from_float"
end

module Event : sig
  type t = [ `Event ] Jx.obj

  external of_any : Jx.any -> t = "%identity"
  external to_any : t -> Jx.any = "%identity"
  val make_with_type : type':string -> unit -> t

  val make_with_type_and_event_init_dict :
    type':string -> event_init_dict:Event_init.t -> unit -> t

  val type' : t -> string
  val target : t -> event_target Jx.nullable
  val src_element : t -> event_target Jx.nullable
  val current_target : t -> event_target Jx.nullable
  val composed_path : t -> event_target Jx.array
  val none : Jx.number
  val capturing_phase : Jx.number
  val at_target : Jx.number
  val bubbling_phase : Jx.number
  val event_phase : t -> int
  val stop_propagation : t -> unit
  val cancel_bubble : t -> bool
  val set_cancel_bubble : t -> bool -> unit
  val stop_immediate_propagation : t -> unit
  val bubbles : t -> bool
  val cancelable : t -> bool
  val return_value : t -> bool
  val set_return_value : t -> bool -> unit
  val prevent_default : t -> unit
  val default_prevented : t -> bool
  val composed : t -> bool
  val is_trusted : t -> bool
  val time_stamp : t -> dom_high_res_time_stamp
  val init_event : type':string -> t -> unit
  val init_event_with_bubbles : type':string -> bubbles:bool -> t -> unit

  val init_event_with_bubbles_and_cancelable :
    type':string -> bubbles:bool -> cancelable:bool -> t -> unit
end = struct
  type t = [ `Event ] Jx.obj

  let t = Jx.expr "Event"

  external of_any : Jx.any -> t = "%identity"
  external to_any : t -> Jx.any = "%identity"

  let make_with_type ~type' () =
    let type' = E.string type' in
    Jx.obj_new t [| type' |]

  let make_with_type_and_event_init_dict ~type' ~event_init_dict () =
    let type' = E.string type' in
    let event_init_dict = Event_init.to_any event_init_dict in
    Jx.obj_new t [| type'; event_init_dict |]

  let type' this = D.field this "type" D.string
  let target this = D.field this "target" D.obj
  let src_element this = D.field this "srcElement" D.obj
  let current_target this = D.field this "currentTarget" D.obj
  let composed_path this = D.field this "composedPath" D.obj
  let none = Jx.int 0
  let capturing_phase = Jx.int 1

  (* let capturing_phase = E.int 1 |> D.obj *)
  let at_target = Jx.int 2
  let bubbling_phase = Jx.int 3
  let event_phase this = D.field this "eventPhase" D.int
  let stop_propagation this = D.unit (D.meth this "stopPropagation" [||])
  let cancel_bubble this = D.field this "cancelBubble" D.bool
  let set_cancel_bubble this x = E.field this "cancelBubble" E.bool x

  let stop_immediate_propagation this =
    D.unit (D.meth this "stopImmediatePropagation" [||])

  let bubbles this = D.field this "bubbles" D.bool
  let cancelable this = D.field this "cancelable" D.bool
  let return_value this = D.field this "returnValue" D.bool
  let set_return_value this x = E.field this "returnValue" E.bool x
  let prevent_default this = D.unit (D.meth this "preventDefault" [||])
  let default_prevented this = D.field this "defaultPrevented" D.bool
  let composed this = D.field this "composed" D.bool
  let is_trusted this = D.field this "isTrusted" D.bool
  let time_stamp this = D.field this "timeStamp" dom_high_res_time_stamp_of_any

  let init_event ~type' this =
    let type' = E.string type' in
    D.unit (D.meth this "initEvent" [| type' |])

  let init_event_with_bubbles ~type' ~bubbles this =
    let type' = E.string type' in
    let bubbles = E.bool bubbles in
    D.unit (D.meth this "initEvent" [| type'; bubbles |])

  let init_event_with_bubbles_and_cancelable ~type' ~bubbles ~cancelable this =
    let type' = E.string type' in
    let bubbles = E.bool bubbles in
    let cancelable = E.bool cancelable in
    D.unit (D.meth this "initEvent" [| type'; bubbles; cancelable |])
end

module Event_listener : sig
  type t = Event.t -> unit

  val of_any : Jx.any -> t
  val to_any : t -> Jx.any
end = struct
  type t = Event.t -> unit

  let to_any f = E.func 1 f

  let of_any any event =
    let event = Event.to_any event in
    D.unit (D.func any [| event |])
end

module Event_target : sig
  type t = [ `Event_target ] Jx.obj

  external of_any : Jx.any -> t = "%identity"
  external to_any : t -> Jx.any = "%identity"
  val make : unit -> t

  val add_event_listener :
    type':string -> callback:event_listener Jx.nullable -> t -> unit

  val add_event_listener_with_options :
    type':string ->
    callback:event_listener Jx.nullable ->
    options:[< `Add_event_listener_options | `Bool ] Jx.obj ->
    t ->
    unit

  val remove_event_listener :
    type':string -> callback:event_listener Jx.nullable -> t -> unit

  val remove_event_listener_with_options :
    type':string ->
    callback:event_listener Jx.nullable ->
    options:[< `Event_listener_options | `Bool ] Jx.obj ->
    t ->
    unit

  val dispatch_event : event:event -> t -> bool
end = struct
  type t = [ `Event_target ] Jx.obj

  let t = Jx.expr "EventTarget"

  external of_any : Jx.any -> t = "%identity"
  external to_any : t -> Jx.any = "%identity"

  let make () = Jx.obj_new t [||]

  let add_event_listener ~type' ~callback this =
    let type' = E.string type' in
    let callback = Jx.Nullable.to_any Event_listener.to_any callback in
    D.unit (D.meth this "addEventListener" [| type'; callback |])

  let add_event_listener_with_options ~type' ~callback ~options this =
    let type' = E.string type' in
    let callback = Jx.Nullable.to_any Event_listener.to_any callback in
    let options = E.any options in
    D.unit (D.meth this "addEventListener" [| type'; callback; options |])

  let remove_event_listener ~type' ~callback this =
    let type' = E.string type' in
    let callback = Jx.Nullable.to_any Event_listener.to_any callback in
    D.unit (D.meth this "removeEventListener" [| type'; callback |])

  let remove_event_listener_with_options ~type' ~callback ~options this =
    let type' = E.string type' in
    let callback = Jx.Nullable.to_any Event_listener.to_any callback in
    let options = E.any options in
    D.unit (D.meth this "removeEventListener" [| type'; callback; options |])

  let dispatch_event ~event this =
    let event = Event.to_any event in
    D.bool (D.meth this "dispatchEvent" [| event |])
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
    of_any (D.meth this "appendChild" [| to_any node |])
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

  let append ~nodes this = D.unit (D.meth this "append" (E.Array.obj nodes))
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
    let data = E.any data in
    Jx.obj_new t [| data |]
end

module Html_collection : sig
  type t = [ `Html_collection ] Jx.obj

  external of_any : Jx.any -> t = "%identity"
  external to_any : t -> Jx.any = "%identity"
  val length : t -> int
  val item : index:int -> t -> Element.t Jx.nullable
end = struct
  type t = [ `Html_collection ] Jx.obj

  let t = Jx.expr "HTMLCollection"

  external of_any : Jx.any -> t = "%identity"
  external to_any : t -> Jx.any = "%identity"

  let length this = D.field this "length" D.int

  let item ~index this =
    let index = E.int index in
    D.obj (D.meth this "item" [| index |])
end

module Document : sig
  type t = [ `Document ] Jx.obj

  external to_node : t -> Node.t = "%identity"
  external of_any : Jx.any -> t = "%identity"
  external to_any : t -> Jx.any = "%identity"
  val make : unit -> t
  val create_element : tag_name:string -> t -> Element.t

  val create_element_with_options :
    tag_name:string ->
    options:[< `String | `Element_creation_options ] Jx.obj ->
    t ->
    Element.t

  val create_text_node : data:string -> t -> Text.t
  val query_selector : selectors:string -> t -> Element.t Jx.nullable
  val children : t -> Html_collection.t
  val import_node : node:Node.t -> t -> Node.t
  val import_node_with_deep : node:Node.t -> deep:bool -> t -> Node.t
end = struct
  type t = [ `Document ] Jx.obj

  let t = Jx.expr "Document"
  let make () = Jx.obj_new t [||]

  external to_node : t -> Node.t = "%identity"
  external of_any : Jx.any -> t = "%identity"
  external to_any : t -> Jx.any = "%identity"

  let create_element ~tag_name this =
    let tag_name = E.string tag_name in
    Element.of_any (D.meth this "createElement" [| tag_name |])

  let create_element_with_options ~tag_name ~options this =
    let tag_name = E.string tag_name in
    let options = E.any options in
    Element.of_any (D.meth this "createElement" [| tag_name; options |])

  let create_text_node ~data this =
    Text.of_any (D.meth this "createTextNode" [| E.string data |])

  let query_selector ~selectors this =
    D.obj (D.meth this "querySelector" [| E.string selectors |])

  let children this = D.field this "children" D.obj

  let import_node ~node this =
    let node = Node.to_any node in
    Node.of_any (D.meth this "importNode" [| node |])

  let import_node_with_deep ~node ~deep this =
    let node = Node.to_any node in
    let deep = E.bool deep in
    Node.of_any (D.meth this "importNode" [| node; deep |])
end

let window : [ `Window ] Jx.obj = D.field Jx.global "window" D.obj

let parse_int_js (str : Jx.string) (radix : Jx.number) : Jx.number =
  D.obj (D.func (Jx.expr "parseInt") [| E.any str; E.any radix |])

let parse_int_ml (str : string) (radix : int) : int =
  D.int (D.func (Jx.expr "parseInt") [| E.string str; E.int radix |])

let alert str = D.unit (D.func (Jx.expr "window.alert") [| E.string str |])
let document = Jx.expr "document" |> Document.of_any

let word_count str =
  let src =
    {|
      function countWords(str) {
        return str.trim().split(/\s+/).length;
      }
    |}
  in
  D.int (D.func (Jx.expr src) [| E.string str |])
