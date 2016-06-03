open Monad
(* List Monad *)

module L : (Monad.S with type 'a m := 'a list) =
  Monad.Make(
      struct
        type 'a m = 'a list
        let return x = [x]
        let bind x f = List.fold_left (fun a x -> a @ f x) [] x
      end)

let cart_prod xs ys =
  L.(xs >>= (fun x ->
    ys >>= (fun y ->
            return (x,y))))
;;
