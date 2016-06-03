(* Parser Monads *)

type 'a parser = string -> ('a * string) list

module ParserMonad : (Monad.S with type 'a m := 'a parser) =
  Monad.Make(
      struct
        open ListMonad.L
        type 'a m = 'a parser

        let return a = fun s -> [(a,s)]

        let bind p f =
          fun s -> (
            p s >>= (fun (a, s') ->
              (f a) s'))

      end)

module ParserApplicative : (Applicative.S with type 'a a := 'a parser)
  = Applicative.FromMonad(ParserMonad)

module ParserFunctor : (Functor.S with type f := ParserApplicative.m)
  = Functor.FromApplicative(ParserApplicative)

open ParserMonad
open ParserApplicative
open ParserFunctor
