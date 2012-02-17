module type T = sig
    type t;;
end;;

module type S = sig
    type data;;
    type 'a t;;
    val return : 'a -> 'a t;;
    val bind : 'a t -> ('a -> 'b t) -> 'b t;;
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t;;
    val seq : 'a t list -> 'a list t;;
    val read : data t;;
    val run : data -> 'a t -> 'a;;
end;;

module Make(Ty : T) : S with type data = Ty.t;;

