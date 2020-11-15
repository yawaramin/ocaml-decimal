(** Integer arithmetic functions used by [ln], [log10], [exp], and [pow]. *)

(* let nbits = Z.numbits *)
let z2 = Z.of_int 2
let z10 = Z.of_int 10
let zeros = Str.regexp "0+$"

(** [decimal_lshift_exact n e] is [Some (n * 10 ** e)] if it's an integer, else
    [None]. *)
let decimal_lshift_exact n e =
  if n = Z.zero then
    Some n
  else if Z.(gt n zero) then
    Some Z.(n * pow z10 e)
  else
    (* val_n = largest power of 10 dividing n. *)
    let str_n = Z.(n |> abs |> to_string) in
    let val_n =
      String.length str_n - String.length (Str.replace_first zeros "" str_n)
    in
    let neg_e = -e in
    if val_n < neg_e then None else Some Z.(n / pow z10 neg_e)

let rec sqrt_nearest n a b =
  if Z.(equal a b) then a
  else
    let neg_n = Z.neg n in
    (sqrt_nearest [@tailcall]) n Z.(shift_right_trunc (a - neg_n /< a) 1) a

(** [sqrt_nearest n a] is the closest integer to the square root of the positive
    integer [n]. [a] is an initial approximation of the square root. Any
    positive integer will do for [a], but the closer [a] is to the square root
    of [n] the faster convergence will be. *)
let sqrt_nearest n a =
  if Z.Compare.(n <= Z.zero || a <= Z.zero) then
    invalid_arg "sqrt_nearest: both arguments should be positive"
  else
    sqrt_nearest n a Z.zero

(** [rshift_nearest x shift] is the closest integer to [x / 2**shift], where
    [shift] is non-negative. Uses round-to-even in case of a tie. *)
let rshift_nearest x shift =
  let open Z in
  let b = one lsl shift in
  let q = x asr shift in
  q + if z2 * x land (b - one) + q land one > b then one else zero

let div_nearest a b =
  let open Z in
  let q, r = div_rem a b in
  q + if z2 * r + q land one > b then one else zero
