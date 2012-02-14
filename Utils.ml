
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

