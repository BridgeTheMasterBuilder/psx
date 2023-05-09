type t

val of_int : int -> t
val to_int : t -> int
val ( land ) : t -> t -> t
val ( lsr ) : t -> t -> t
val ( + ) : t -> t -> t
val ( / ) : t -> t -> t
val ( * ) : t -> t -> t
val bit : t -> t -> t
val bits : t -> t -> t -> t
val bits_abs : t -> t -> t -> t
