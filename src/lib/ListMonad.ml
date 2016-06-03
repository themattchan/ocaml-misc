(* List Monad *)

module M : (Monad.S with type 'a m := 'a list) =
  Monad.Make(
      struct
        type 'a m = 'a list
        let return x = [x]
        let bind x f = List.fold_left (fun a x -> a @ f x) [] x
      end)

module A = Applicative.FromMonad(M)
module F = Functor.FromApplicative(A)

let cart_prod xs ys =
  M.(xs >>= (fun x ->
       ys >>= (fun y ->
               return (x,y))))
;;
