open struct
  module E = Jx.Encode
  module D = Jx.Decode
end

let user =
  Jx.obj
    [|
      ("name", E.string "Ada");
      ("score", E.int 42);
      ("tags", E.array [| E.string "admin"; E.string "active" |]);
    |]

let scores = Jx.obj [| ("math", E.int 42); ("logic", E.int 99) |]
let math = Jx.expr "Math"
let math_max a b = Jx.obj_call math "max" [| E.int a; E.int b |]
let x10_f = Jx.expr "x10"
let x10 n = Jx.call x10_f [| E.int n |]

let () =
  Jx.log user;
  Jx.log scores;
  Jx.log (math_max 4 15);
  Jx.log (math_max 10 (x10 54321 |> D.int))
