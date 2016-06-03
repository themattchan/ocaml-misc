open Monad

module type B = sig
  type _ a
  val pure : 'a -> 'a a
  val (<*>) : ('a -> 'b) a -> 'a a -> 'b a
end

module type S = sig
  type _ a
  include B with type 'a a := 'a a
end

module Make(A : B)
       : (S with type 'a a := 'a A.a)
  =
  struct
    include A
  end

module FromMonad(M : Monad.S)
       : (S with type 'a a := 'a M.m) =
  Make(struct
        open M
        type 'a a = 'a M.m
        let pure = M.return
        let (<*>) af aa =
          af >>= (fun f ->
            aa >>= (fun a ->
                    return (f a)))
      end)
