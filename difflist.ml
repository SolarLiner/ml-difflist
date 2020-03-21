(** Difference List implementation for OCaml.

    {1 Use cases}

    List concatenation in OCaml can be a nightmare. This little modules aims to provides a replacement for the list type for use in concatenation-heavy programs. Optimization are performed using so called Difference List.

    {1 Benefits}

    Difflist gives a O(1) function for list concatenation. This function is lazy : the content of the list is opaque until final conversion to standard ocaml list *)

(** Type for Difference List *)
type ('a, 'b) dlist = DList of ('a list -> 'b list)

(** Alias for Difference List *)
type ('a, 'b) t = ('a, 'b) dlist

(** The empty list *)
let empty = DList(fun xs -> xs)

(** Singleton. ([snoc x] is [[x]]) *)
let snoc x = DList(fun xs -> x::xs)

(** Transformed singleton. ([snoct t x] is [[t x]]). Because of Lasy evaluation, the application of [t] on [x] is performed ONLY during the final conversion (see {!to_list})
    @param  f   Transformation function
    @param  x   Element
*)
let snoct f x = DList(fun xs -> (f x)::xs)

(** List concatenation.
    @param  f   A Difference List
    @param  g   A Difference List
*)
let append f g =
  match (f,g) with
  | (DList f, DList g) -> DList(fun xs -> f (g xs))

(** Build a difference list from a list *)
let rec of_list =
  function
  | [] -> empty
  | x::xs -> append (snoc x) (of_list xs)

(** Build a list from a difference list. Performs every computation delayed for   lazyness *)
let to_list =
  function DList f -> f []

(** Concatenate many difference list
    @param l  A list of difference list to concatenate *)
let concat l =
  List.fold_left append empty l

(** Alias for {!append} *)
let (++) a b = append a b

(** [d <@ l] concatenate a difference list with a list *)
let (<@) d l = append d (of_list l)

(** [d <+ x] append an element (rights) to a difference list *)
let (<+) d x = append d (snoc x)

let (+>) x d = append (snoc x) d

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
