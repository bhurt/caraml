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

let id x = x;;

let take n lst =
    if (n < 0) then
        failwith "Utils.take: n < 0"
    else if n == 0 then
        []
    else
        let rec loop acc n = function
            | [] -> lst
            | x :: xs ->
                if n == 1 then
                    List.rev_append acc [ x ]
                else
                    loop (x :: acc) (n - 1) xs
        in
        loop [] n lst
;;

let rec drop n lst =
    if (n < 0) then
        failwith "Utils.drop: n < 0"
    else if (n == 0) then
        lst
    else
        match lst with
        | [] -> []
        | _ :: xs -> drop (n-1) xs
;;

let take_drop n lst =
    if (n < 0) then
        failwith "Utils.take: n < 0"
    else if n == 0 then
        ([], lst)
    else
        let rec loop acc n = function
            | [] -> (lst, [])
            | x :: xs ->
                if n == 1 then
                    ((List.rev_append acc [ x ]), xs)
                else
                    loop (x :: acc) (n - 1) xs
        in
        loop [] n lst
;;

let rec last = function
    | [] -> failwith "last called on an empty list"
    | [x] -> x
    | _ :: xs -> last xs
;;

let repeat n x =
    if (n < 0) then
        failwith "Utils.repeat: n < 0"
    else
        let rec loop n acc =
            if (n == 0) then
                acc
            else
                loop (n-1) (x :: acc)
        in
        loop n []
;;

let unfold_left f init =
    let rec loop acc init =
        match f init with
        | None -> List.rev acc
        | Some(x, init) -> loop (x :: acc) init
    in
    loop [] init
;;

let unfold_right f init =
    let rec loop acc init =
        match f init with
        | None -> acc
        | Some(init, x) -> loop (x :: acc) init
    in
    loop [] init
;;

let unfoldi f len =
    let rec loop acc i =
        if (i == 0) then
            (f i) :: acc
        else
            loop ((f i) :: acc) (i - 1)
    in
    if (len < 0) then
        failwith "unfoldi: negative length"
    else if (len == 0) then
        []
    else
        loop [] (len - 1)
;;

let range ?(start=0) ?(step=1) ~length =
    let rec loop acc i n =
        if (n == 0) then
            List.rev acc
        else
            loop (i :: acc) (i + step) (n - 1)
    in
    if length < 0 then
        failwith "range: negative length!"
    else
        loop [] start length
;;

let rec mapi ?(start=0) ?(step=1) f lst =
    let rec loop ys i = function
        | [] -> List.rev ys
        | x :: xs ->
            let y = f i x in
            loop (y :: ys) (i + step) xs
    in
    loop [] start lst
;;

let fold_lefti ?(start=0) ?(step=1) f init lst =
    let rec loop i acc = function
        | [] -> acc
        | x :: xs -> loop (i + step) (f i acc x) xs
    in
    loop start init lst
;;

let fold_righti ?(start=0) ?(step=1) f lst init =
    let rec loop i acc = function
        | [] -> acc
        | x :: xs -> f i x (loop (i + step) acc xs)
    in
    loop start init lst
;;

let fold_right2i ?(start=0) ?(step=1) f alst blst init =
    let rec loop i acc xs ys =
        match (xs, ys) with
        | [], [] -> acc
        | (x :: xs), (y :: ys) -> f i x y (loop (i + step) acc xs ys)
        | _,_ -> failwith "fold_right2i: lists of unequal lengths"
    in
    loop start init alst blst
;;

let rec freduce fs init =
    match fs with
    | [] -> init
    | g :: gs -> freduce gs (g init)
;;

let map_accum f init xs =
    let rec loop acc ys = function
        | [] -> acc, List.rev ys
        | x :: xs ->
            let acc, y = f acc x in
            loop acc (y :: ys) xs
    in
    loop init [] xs
;;

let map2_accum f init xs ys =
    let rec loop acc zs xs ys =
        match xs, ys with
        | [], [] -> acc, List.rev zs
        | (x :: xs), (y :: ys) ->
            let acc, z = f acc x y in
            loop acc (z :: zs) xs ys
        | _, _ -> failwith "map2_accum: different length lists"
    in
    loop init [] xs ys
;;

let mapcat f xs =
    let rec loop acc = function
        | x :: xs -> loop (List.rev_append (f x) acc) xs
        | [] -> List.rev acc
    in
    loop [] xs
;;

