(* Parser Monads *)

type 'a parser = string -> ('a * string) list

module M : (Monad.S with type 'a m := 'a parser) =
  Monad.Make(
      struct
        open ListMonad.M
        type 'a m = 'a parser

        let return a = fun s -> [(a,s)]

        let bind p f =
          fun s -> (
            p s >>= (fun (a, s') ->
              (f a) s'))

      end)

module A : (Applicative.S with type 'a a := ParserMonad.m) = Applicative.FromMonad(M)
module F = Functor.FromApplicative(A)

open M
open A
open F
