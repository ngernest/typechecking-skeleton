# Typechecking Project Skeleton 

- Adapted from [Kiran Gopinathan's](https://kirancodes.me/index.html) typechecking project skeleton ([source](https://gitlab.com/-/snippets/2162015))

From [this OCaml forum post](https://discuss.ocaml.org/t/writing-type-inference-algorithms-in-ocaml/8191/15?u=ngernest):
> This repo has a helper PPX that also converts OCaml syntax into corresponding values of the following ADT:
> 
> ```ocaml
> type term =
>     Var of int
>   | Lam of term
>   | App of term * term
>   | Let of term * term
> ```
> i.e
> ```ocaml
> let%syntax f x y = y x 
> ```
> becomes:
> ```ocaml
> let f  = 
>    Lam (Lam (App (Var 0, Var 1)))
> ```
> which was quite useful when going through Oleg’s article, as it allowed me to check the > type-checking algorithm at each step against OCaml’s one by simply removing the “%syntax” > annotation and checking the OCaml type for the term.
> 