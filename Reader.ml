(*
    Caraml compiler
    Copyright (C) 2012 Brian Hurt (bhurt@spnz.org)

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

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

