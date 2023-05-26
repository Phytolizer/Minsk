let value =
  let pp_value ppf x = Fmt.pf ppf "%s" (Minsk.Runtime.Value.show x) in
  Alcotest.testable pp_value ( = )
