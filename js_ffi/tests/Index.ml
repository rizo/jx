open Js_lib

let str_unicode_1 =
  String.concat " " [ "Olá"; "eles"; "são"; "uma"; "ameaça!"; "👻" ]

let str_ascii_1 =
  String.concat " " [ "All"; "your"; "base"; "are"; "belong"; "to"; "us" ]

(* NOTE: In the examples below [str ^ ""] is used to avoid implicit optimization of literal strings. *)
let () =
  Jx.debug "-- Value representation --";
  Jx.debug ();
  Jx.debug 42;
  (* Jx.debug (List.length [ 1; 2 ]); *)
  (* Jx.debug 3.14; *)
  Jx.debug 'x';
  Jx.debug "str1";
  Jx.debug true;
  Jx.debug (1, 2);
  Jx.debug [ 1; 2; 3 ];
  Jx.debug [| 1; 2; 3 |];
  Jx.debug None;
  Jx.debug (Some 1);
  Jx.debug `hello;

  Jx.debug "-- Strings --";
  let str_unicode_1' = Jx.utf16_of_utf8 str_unicode_1 in
  let str_ascii_1' = Jx.utf16_of_utf8 str_ascii_1 in
  Jx.debug str_ascii_1;
  Jx.debug str_ascii_1';
  Jx.debug str_unicode_1';
  let dict = Jx.obj [||] in
  Jx.set dict str_ascii_1 (Jx.int 101);
  Jx.set dict str_unicode_1' (Jx.int 102);
  Jx.set dict str_ascii_1' (Jx.int 103);
  Jx.debug dict;

  Jx.debug "-- Embed JavaScript --";
  let _optimized_away = Jx.expr {|2 + 2|} in
  Jx.debug (Jx.expr {|2 > 1 ? "static expr" : "no"|});
  Jx.stmt "console.log('raw expr')";
  Jx.stmt ("console.log" ^ "('raw expr 2')");
  Jx.debug word_count;
  Jx.debug (word_count str_ascii_1);

  Jx.debug "-- Function bindings --";
  Jx.debug (parse_int_js (Jx.ascii "42") (Jx.int 10));
  Jx.debug (parse_int_ml ("42" ^ "") 10);

  Jx.debug "Date constructor";
  Jx.debug (Date.make_with_value ~value:(Jx.ascii "2024-04-12") ());
  Jx.debug (Date.make_with_value ~value:(Jx.float 321321321.2) ());
  Jx.debug (Date.make ());

  let body =
    Document.query_selector ~selectors:"body" document |> Jx.Nullable.unsafe_get
  in

  Jx.debug "Default arg";
  let _ = Document.import_node ~node:(Element.to_node body) document in
  let _ =
    Document.import_node_with_deep ~node:(Element.to_node body) ~deep:true
      document
  in

  (* let _2 = Document.import_node_2 ~node:body document in
     let _2 = Document.import_node_2 ~node:body ~deep:true document in

     let _3 = Document.import_node_3 ~node:body document in
     let _3 = Document.import_node_3 ~node:body ~deep:(Jx.defined true) document in *)

  (* let _ = Document.import_node' ~node:body document in *)

  (* Abstract dict 1 *)
  (* Generates an object with a single key *)
  let ei = Event_init.empty () in
  Event_init.set_composed ei true;
  Jx.debug ei;

  (* Abstract dict 2 *)
  (* Generates the full object with undefined entries *)
  (* let ei = Event_init.make' ~composed:(Jx.defined true) () in
     Jx.debug ei; *)
  let incr =
    let elem =
      Element.to_node (Document.create_element ~tag_name:"button" document)
    in
    let incr_txt =
      Text.to_node
        (Document.create_text_node ~data:(Jx.utf16_of_utf8 "Incr ➕") document)
    in
    let _ = Node.append_child ~node:incr_txt elem in
    elem
  in
  let decr =
    let elem =
      Element.to_node
        (Document.create_element_with_options ~options:(Jx.ascii "button")
           ~tag_name:"button" document)
    in
    let decr_txt =
      Text.to_node (Text.make_with_data ~data:(Jx.ascii "Decr") ())
    in
    let _ = Node.append_child ~node:decr_txt elem in
    elem
  in
  let reset =
    let elem =
      Element.to_node (Document.create_element ~tag_name:"button" document)
    in
    let reset_txt =
      Text.to_node (Document.create_text_node ~data:"Reset" document)
    in
    let _ = Node.append_child ~node:reset_txt elem in
    elem
  in
  Element.append ~nodes:[| incr; decr; reset |] body;
  Jx.debug (Document.children document);
  ()
