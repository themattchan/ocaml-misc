open Applicative

module type B = sig
  type _ f
  val fmap : ('a -> 'b) -> 'a f -> 'b f
end

module type S = sig
  type _ f
  include B with type 'a f := 'a f
end

module Make(F : B)
       : (S with type 'a f := 'a F.f)
  = struct
  include F
end

module FromApplicative(A : Applicative.S)
       : (S with type 'a f := 'a A.a) =
  Make(struct
        open A
        type 'a f = 'a A.a
        let fmap f c = pure f <*> c
      end)
