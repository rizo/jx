open struct
  module E = Jx.Encode
  module D = Jx.Decode
end

let document = Jx.global "document"

let create_element tag_name =
  Jx.obj_call document "createElement" [| E.string tag_name |]

let create_text_node text = Jx.obj_call document "createTextNode" [| text |]

let get_element_by_id id =
  let element = Jx.obj_call document "getElementById" [| E.string id |] in
  if Jx.strict_equal element Jx.null then None else Some element

let append_child parent child =
  ignore (Jx.obj_call parent "appendChild" [| child |])

let append_text parent text = append_child parent (create_text_node text)

let set_attribute element name value =
  ignore
    (Jx.obj_call element "setAttribute" [| E.string name; E.string value |])

let remove_attribute element name =
  ignore (Jx.obj_call element "removeAttribute" [| E.string name |])

let set_class_name element class_name =
  Jx.obj_set element "className" (E.string class_name)

let set_text_content element text = Jx.obj_set element "textContent" text
let clear element = set_text_content element (E.string "")
let value input = Jx.obj_get input "value"
let set_value input value = Jx.obj_set input "value" value
let checked input = D.bool (Jx.obj_get input "checked")
let set_checked input checked = Jx.obj_set input "checked" (E.bool checked)

let set_disabled element disabled =
  Jx.obj_set element "disabled" (E.bool disabled)

let add_event_listener element event_name handler =
  ignore
    (Jx.obj_call element "addEventListener"
       [| E.string event_name; E.func 1 handler |])

let prevent_default event = ignore (Jx.obj_call event "preventDefault" [||])
let focus element = ignore (Jx.obj_call element "focus" [||])

module String = struct
  let trim text = Jx.obj_call text "trim" [||]
  let is_empty text = Jx.strict_equal text (E.string "")
end
