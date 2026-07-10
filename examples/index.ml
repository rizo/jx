let user =
  Jx.obj
    [|
      ("name", Jx.string "Ada");
      ("score", Jx.int 42);
      ("tags", Jx.array [| Jx.string "admin"; Jx.string "active" |]);
    |]

let scores = Jx.obj [| ("math", Jx.int 42); ("logic", Jx.int 99) |]
let math = Jx.expr "Math"
let math_max a b = Jx.meth math "max" [| Jx.int a; Jx.int b |]
let x10_f = Jx.expr "x10"
let x10 n = Jx.call x10_f [| Jx.int n |]

let () =
  Jx.log user;
  Jx.log scores;
  Jx.log (math_max 4 15);
  Jx.log (math_max 10 (x10 54321 |> Jx.Ml.int))
