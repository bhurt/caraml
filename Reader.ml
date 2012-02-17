
module type T = sig
    type t;;
end;;

module type S = sig
    type data;;

    type 'a t;;

    val return: 'a -> 'a t;;
    val bind : 'a t -> ('a -> 'b t) -> 'b t;;
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t;;
    val seq : 'a t list -> 'a list t;;

    val read : data t;;
    val run : data -> 'a t -> 'a;;

end;;

module Make(Ty: T) : S with type data = Ty.t = struct

    type data = Ty.t;;

    type 'a t = (data -> 'a);;

    let return x = fun _ -> x;;

    let bind x f = fun data -> f (x data) data;;

    let ( >>= ) = bind;;

    let rec seq = function
        | [] -> return []
        | m :: ms ->
            perform
                m' <-- m;
                ms' <-- seq ms;
                return (m' :: ms')
    ;;

    let read : data t = (fun d -> d);;

    let run data f = f data;;

end;;

