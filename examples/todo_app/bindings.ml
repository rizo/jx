open struct
  module E = Jx.Encode
  module D = Jx.Decode
end

let document = Jx.global "document"

let create_element tag_name =
  Jx.meth document "createElement" [| E.string tag_name |]

let create_text_node text = Jx.meth document "createTextNode" [| text |]

let get_element_by_id id =
  let element = Jx.meth document "getElementById" [| E.string id |] in
  if Jx.strict_equal element Jx.null then None else Some element

let append_child parent child =
  ignore (Jx.meth parent "appendChild" [| child |])

let append_text parent text = append_child parent (create_text_node text)

let set_attribute element name value =
  ignore (Jx.meth element "setAttribute" [| E.string name; E.string value |])

let remove_attribute element name =
  ignore (Jx.meth element "removeAttribute" [| E.string name |])

let set_class_name element class_name =
  Jx.set element "className" (E.string class_name)

let set_text_content element text = Jx.set element "textContent" text
let clear element = set_text_content element (E.string "")
let value input = Jx.get input "value"
let set_value input value = Jx.set input "value" value
let checked input = D.bool (Jx.get input "checked")
let set_checked input checked = Jx.set input "checked" (E.bool checked)
let set_disabled element disabled = Jx.set element "disabled" (E.bool disabled)

let add_event_listener element event_name handler =
  ignore
    (Jx.meth element "addEventListener"
       [| E.string event_name; E.func 1 handler |])

let prevent_default event = ignore (Jx.meth event "preventDefault" [||])
let focus element = ignore (Jx.meth element "focus" [||])

module String = struct
  let trim text = Jx.meth text "trim" [||]
  let is_empty text = Jx.strict_equal text (E.string "")
end
