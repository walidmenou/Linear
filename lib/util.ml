let ltos cs = cs |> List.to_seq |> String.of_seq
let fst (a, _) = a
let explode s = s |> String.to_seq |> List.of_seq
