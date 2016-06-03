(* "Typeclasses" as Modules and Functors
 * (can also do dictionary passing with record types...)
 *)


(* Monad > Applicative > Functor
 *
 * Class A extends Class B if B can be implemented with A's primitives
 *)

(* The basic stuff *)
module type B = sig
  type _ m
  val return : 'a -> 'a m
  val bind   : 'a m -> ('a -> 'b m) -> 'b m
end

(*  Things defined in terms of return, bind *)
module type S = sig
  type _ m
  include B with type 'a m := 'a m

  val (>>=) : 'a m -> ('a -> 'b m) -> 'b m
  val (<<=) : ('a -> 'b m) -> 'a m -> 'b m

  val (>=>) : ('a -> 'b m) -> ('b -> 'c m) -> ('a -> 'c m)
  val (<=<) : ('b -> 'c m) -> ('a -> 'b m) -> ('a -> 'c m)

  val join : ('a m) m -> 'a m
end

(* Default implementations *)
module Make(M : B) : (S with type 'a m = 'a M.m) = struct
  include M

  let (>>=) m f = M.bind m f
  let (<<=) f m = m >>= f

  let (>=>) f g = fun x -> f x >>= g
  let (<=<) g f = f >=> g

  let join mm = mm >>= (fun x -> x)
end
