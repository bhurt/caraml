val take : int -> 'a list -> 'a list;;
val drop : int -> 'a list -> 'a list;;
val take_drop : int -> 'a list -> 'a list * 'a list;;
val repeat : int -> 'a -> 'a list;;
val unfold_left : ('a -> ('b * 'a) option) -> 'a -> 'b list;;
val unfold_right : ('a -> ('a * 'b) option) -> 'a -> 'b list;;
