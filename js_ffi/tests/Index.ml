module Js = Js_ffi
module D = Js.Decode
module E = Js.Encode

module Node : sig
  type self = [ `Node ]
  type super = [ self | `Event_target ]
  type t = self Js.obj

  external of_js : Js.t -> [< super ] Js.obj = "%identity"
  external to_js : [> self ] Js.obj -> Js.t = "%identity"

  val append_child :
    node:[> self ] Js.obj -> [> self ] Js.obj -> [< super ] Js.obj
end = struct
  type self = [ `Node ]
  type super = [ self | `Event_target ]
  type t = self Js.obj

  external of_js : Js.t -> [< super ] Js.obj = "%identity"
  external to_js : [> self ] Js.obj -> Js.t = "%identity"

  let append_child ~node this =
    of_js (Js.meth_call this "appendChild" [| to_js node |])
end

module Element : sig
  type self = [ `Element ]
  type super = [ self | Node.super ]
  type t = self Js.obj

  external of_js : Js.t -> [< super ] Js.obj = "%identity"
  external to_js : [> self ] Js.obj -> Js.t = "%identity"
end = struct
  type self = [ `Element ]
  type super = [ self | Node.super ]
  type t = self Js.obj

  external of_js : Js.t -> [< super ] Js.obj = "%identity"
  external to_js : [> self ] Js.obj -> Js.t = "%identity"
end

module Text : sig
  type self = [ `Text ]
  type super = [ self | Node.super ]
  type t = self Js.obj

  external of_js : Js.t -> [< super ] Js.obj = "%identity"
  external to_js : [> self ] Js.obj -> Js.t = "%identity"
  val make : ?data:string -> unit -> [< super ] Js.obj
end = struct
  type self = [ `Text ]
  type super = [ self | Node.super ]
  type t = self Js.obj

  let t = Js.raw "Text"

  external of_js : Js.t -> [< super ] Js.obj = "%identity"
  external to_js : [> self ] Js.obj -> Js.t = "%identity"

  let make ?data () =
    let data = E.option_as_undefined E.string data in
    Js.obj_new t [| data |]
end

module Html_collection : sig
  type self = [ `Html_collection ]
  type super = self
  type t = self Js.obj

  external of_js : Js.t -> [< super ] Js.obj = "%identity"
  external to_js : [> self ] Js.obj -> Js.t = "%identity"
  val length : [> self ] Js.obj -> int
  val item : index:int -> t -> [< Element.super ] Js.obj option
end = struct
  type self = [ `Html_collection ]
  type super = self
  type t = self Js.obj

  let t = Js.raw "HTMLCollection"

  external of_js : Js.t -> [< super ] Js.obj = "%identity"
  external to_js : [> self ] Js.obj -> Js.t = "%identity"

  let length this = D.int (Js.get this "length")

  let item ~index this =
    D.nullable_as_option Element.of_js
      (Js_ffi.meth_call this "item" [| E.int index |])
end

module Document : sig
  type self = [ `Document ]
  type super = [ self | Node.super ]
  type t = self Js.obj

  external super : t -> [< super ] Js.obj = "%identity"
  external of_js : Js.t -> [< super ] Js.obj = "%identity"
  external to_js : [> self ] Js.obj -> Js.t = "%identity"
  val make : unit -> [< super ] Js.obj

  val create_element :
    local_name:string ->
    ?options:[< `String | `Element_creation_options ] Js.obj ->
    [> self ] Js.obj ->
    [< Element.super ] Js.obj

  val create_text_node :
    data:string -> [> self ] Js.obj -> [< Text.super ] Js.obj

  val query_selector :
    selectors:string -> [> self ] Js.obj -> [< Element.super ] Js.obj option

  val append :
    nodes:[< `Node | `Trusted_script | `String ] Js.obj array ->
    [> self ] Js.obj ->
    unit

  val children : [> self ] Js.obj -> Html_collection.t
end = struct
  type self = [ `Document ]
  type super = [ self | Node.super ]
  type t = self Js.obj

  let t = Js.raw "Document"
  let make () = Js_ffi.obj_new t [||]

  external super : t -> [< super ] Js.obj = "%identity"
  external of_js : Js.t -> [< super ] Js.obj = "%identity"
  external to_js : [> self ] Js.obj -> Js.t = "%identity"

  let create_element ~local_name ?options this =
    let options = E.option_as_undefined E.obj options in
    Element.of_js
      (Js.meth_call this "createElement"
         [| E.string_ascii local_name; options |])

  let create_text_node ~data this =
    Text.of_js (Js.meth_call this "createTextNode" [| E.string_ascii data |])

  let query_selector ~selectors this =
    D.nullable_as_option Element.of_js
      (Js.meth_call this "querySelector" [| E.string_ascii selectors |])

  let append ~nodes this =
    D.unit (Js_ffi.meth_call this "append" [| Obj.magic nodes |])

  let children this = Html_collection.of_js (Js.get this "children")
end

let parse_int1 str base =
  D.int (Js.fun_call (Js.raw "parseInt") [| E.string_ascii str; E.int base |])

let document = Js.raw "document" |> Document.of_js

let () =
  Js.debug "starting...";
  let x1 = parse_int1 "42" 10 in
  Js.debug x1;
  let body = Document.query_selector ~selectors:"body" document |> Option.get in
  let incr =
    let elem = Document.create_element ~local_name:"button" document in
    let incr_txt = Document.create_text_node ~data:"Incr" document in
    let _ = Node.append_child ~node:incr_txt elem in
    elem
  in
  let decr =
    let elem = Document.create_element ~local_name:"button" document in
    let decr_txt = Text.make ~data:"Decr" () in
    let _ = Node.append_child ~node:decr_txt elem in
    elem
  in
  let _ = Node.append_child ~node:incr body in
  let _ = Node.append_child ~node:decr body in
  Js.debug (Document.children document);
  ()

module Test1 = struct
  let decode_complex_1 (js : Js.t) =
    D.undefined (D.array (D.nullable D.float)) js
end
