open struct
  module B = Bindings
  module E = Jx.Encode
end

type todo = { id : int; text : Jx.t; mutable completed : bool }

let todos = ref []
let next_id = ref 0

let get_root () =
  match B.get_element_by_id "app" with
  | Some root -> root
  | None -> failwith "missing #app element"

let item_label count singular plural = if count = 1 then singular else plural

let summary_text total remaining =
  let items = item_label total "item" "items" in
  let tasks = item_label remaining "task" "tasks" in
  String.concat ""
    [
      string_of_int total;
      " ";
      items;
      ",";
      " ";
      string_of_int remaining;
      " ";
      tasks;
    ]

let create_button ?(class_name = "") label =
  let button = B.create_element "button" in
  B.set_attribute button "type" "button";
  B.set_class_name button class_name;
  B.append_text button (E.string label);
  button

let create_app root =
  let shell = B.create_element "section" in
  let title = B.create_element "h1" in
  let form = B.create_element "form" in
  let input = B.create_element "input" in
  let add_button = B.create_element "button" in
  let summary = B.create_element "p" in
  let list = B.create_element "ul" in
  let clear_completed =
    create_button ~class_name:"secondary" "Clear completed"
  in
  B.set_class_name shell "todo-app";
  B.append_text title (E.string "Todo");
  B.set_class_name form "todo-form";
  B.set_attribute input "type" "text";
  B.set_attribute input "autocomplete" "off";
  B.set_attribute input "placeholder" "Add a task";
  B.set_attribute input "aria-label" "New todo";
  B.set_attribute add_button "type" "submit";
  B.append_text add_button (E.string "Add");
  B.set_class_name summary "todo-summary";
  B.set_class_name list "todo-list";
  B.append_child form input;
  B.append_child form add_button;
  B.append_child shell title;
  B.append_child shell form;
  B.append_child shell summary;
  B.append_child shell list;
  B.append_child shell clear_completed;
  B.append_child root shell;
  (input, add_button, summary, list, clear_completed)

let add_todo text =
  let todo = { id = !next_id; text; completed = false } in
  incr next_id;
  todos := todo :: !todos

let remove_todo id = todos := List.filter (fun todo -> todo.id <> id) !todos
let has_completed () = List.exists (fun todo -> todo.completed) !todos

let remaining_count () =
  List.fold_left
    (fun count todo -> if todo.completed then count else count + 1)
    0 !todos

let () =
  let input, add_button, summary, list, clear_completed =
    create_app (get_root ())
  in
  let rec render () =
    B.clear list;
    let total = List.length !todos in
    let remaining = remaining_count () in
    B.set_text_content summary (E.string (summary_text total remaining));
    B.set_disabled clear_completed (not (has_completed ()));
    match !todos with
    | [] ->
      let empty = B.create_element "li" in
      B.set_class_name empty "todo-empty";
      B.append_text empty (E.string "Nothing to do yet.");
      B.append_child list empty
    | todos -> List.iter render_todo (List.rev todos)
  and render_todo todo =
    let item = B.create_element "li" in
    let checkbox = B.create_element "input" in
    let label = B.create_element "span" in
    let remove = create_button ~class_name:"danger" "Remove" in
    B.set_class_name item
      (if todo.completed then "todo-item completed" else "todo-item");
    B.set_attribute checkbox "type" "checkbox";
    B.set_attribute checkbox "aria-label" "Toggle todo";
    B.set_checked checkbox todo.completed;
    B.append_text label todo.text;
    B.add_event_listener checkbox "change" (fun _event ->
        todo.completed <- B.checked checkbox;
        render ());
    B.add_event_listener remove "click" (fun _event ->
        remove_todo todo.id;
        render ());
    B.append_child item checkbox;
    B.append_child item label;
    B.append_child item remove;
    B.append_child list item
  in
  B.add_event_listener clear_completed "click" (fun _event ->
      todos := List.filter (fun todo -> not todo.completed) !todos;
      render ());
  B.add_event_listener input "input" (fun _event ->
      let text = B.String.trim (B.value input) in
      B.set_disabled add_button (B.String.is_empty text));
  B.add_event_listener (Jx.obj_get input "form") "submit" (fun event ->
      B.prevent_default event;
      let text = B.String.trim (B.value input) in
      if not (B.String.is_empty text) then (
        add_todo text;
        B.set_value input (E.string "");
        B.set_disabled add_button true;
        B.focus input;
        render ()));
  B.set_disabled add_button true;
  render ()
