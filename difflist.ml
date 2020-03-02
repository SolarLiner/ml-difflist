type ('a, 'b) dlist = DList of ('a list -> 'b list)
type ('a, 'b) t = ('a, 'b) dlist

let empty = DList(fun xs -> xs)
let snoc x = DList(fun xs -> x::xs)
let snoct f x = DList(fun xs -> (f x)::xs)
let append f g =
  match (f,g) with
  | (DList f, DList g) -> DList(fun xs -> f (g xs))

let rec of_list =
  function
  | [] -> empty
  | x::xs -> append (snoc x) (of_list xs)

let rec of_list_t f =
  function
  | [] -> empty
  | x::xs -> append (snoc (f x)) (of_list_t f xs)

let to_list =
  function DList f -> f []

let concat l =
  List.fold_left append empty l

let (++) a b = append a b
let (<+) d l = append d (of_list l)
let (<@) d x = append d (snoc x)

let map f =
  function DList fl -> DList(fun xs -> List.map f (fl xs))
let fold_right f l acc =
  match l with DList fl -> List.fold_right f (fl []) acc
let fold_left f acc =
  function DList fl -> List.fold_left f acc (fl [])

let%test "empty" = match empty |> to_list with
  | [] -> true
  | _ -> false
let%test "singleton" = match snoc 1 |> to_list with
  | [1] -> true
  | _ -> false
let%test "map" =
  let l = of_list ['h';'e';'l';'l';'o'] |> map Char.uppercase_ascii |> to_list in
  let s = String.init (List.length l) (List.nth l) in
  match s with
  | "HELLO" -> true
  | _ -> false
let%test "fold" =
  let l = of_list ['h';'e';'l';'l';'o'] in
  let s = fold_right (fun c acc -> (String.make 1 c) ^ acc) l "" in
  match s with
  | "hello" -> true
  | _ -> false
let%test "concat-2" = match concat [snoc 1; snoc 2] |> to_list with
  | [1; 2] -> true
  | _ -> false
let%test "concat-empty" = match concat [] |> to_list with
  | [] -> true
  | _ -> false
let%test "concat-n" = match concat [of_list [1; 2; 3]; of_list [4; 5; 6]] |> to_list with
  | [1; 2; 3; 4; 5; 6] -> true
  | _ -> false
