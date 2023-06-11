# jx

Work with JavaScript values in OCaml.


## Examples

```
(* Bind the global document value. *)
let document = Jx.global "document"

let get_element_by_id id =
  (* Define decoder for the return type. *)
  let return = Jx.Decoder.(nullable js) in
  (* Bind the method call: document.getElementById(id). *)
  Jx.Obj.call1 document "getElementById" Jx.Encoder.string id ~return
```
