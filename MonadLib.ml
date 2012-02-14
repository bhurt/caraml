
module type Base = sig
    type 'a t;;
    val return : 'a -> 'a t;;
    val bind : 'a t -> ('a -> 'b t) -> 'b t;;
end;;

module type S = sig
    include Base;;

    val ( >>= ) : 'a t -> ( 'a -> 'b t) -> 'b t;;
    val seq : 'a t list -> 'a list t;;
end;;

module Make(B: Base) : S with type 'a t = 'a B.t = struct

    include B;;

    let ( >>= ) = bind;;

    let rec seq : 'a t list -> 'a list t = function
        | [] -> return []
        | m :: ms ->
            perform
                m' <-- m;
                ms' <-- seq ms;
                return (m' :: ms')
    ;;

end;;

