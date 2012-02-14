module type Base = sig
    type 'a t;;
    val return : 'a -> 'a t;;
    val bind : 'a t -> ('a -> 'b t) -> 'b t;;
end;;

module type S = sig
    type 'a t;;
    val return : 'a -> 'a t;;
    val bind : 'a t -> ('a -> 'b t) -> 'b t;;
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t;;
    val seq : 'a t list -> 'a list t;;
end;;

module Make(B: Base) : S with type 'a t = 'a B.t;;

