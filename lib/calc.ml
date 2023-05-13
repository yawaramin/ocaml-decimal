(** Integer arithmetic functions used by [ln], [log10], [exp], and [pow]. *)

(* let nbits = Z.numbits *)
let z2 = Z.of_int 2
let z10 = Z.of_int 10
let z100 = Z.of_int 100
let zeros = Str.regexp "0+$"

(** Unreliable digits at the end of the [log10_digits] calculation *)
let unreliable = Str.regexp "[^0]0*$"

(** [decimal_lshift_exact n e] is [Some (n * 10 ** e)] if it's an integer, else
    [None]. *)
let decimal_lshift_exact n e =
  if Z.(equal n zero) then
    Some n
  else if Z.(gt n zero) then
    Some Z.(n * pow z10 e)
  else (* val_n = largest power of 10 dividing n. *)
    let str_n = Z.(n |> abs |> to_string) in
    let val_n =
      String.length str_n - String.length (Str.replace_first zeros "" str_n)
    in
    let neg_e = -e in
    if val_n < neg_e then None else Some Z.(n / pow z10 neg_e)

let rec sqrt_nearest n a b =
  if Z.(equal a b) then
    a
  else
    let neg_n = Z.neg n in
    (sqrt_nearest [@tailcall]) n Z.(shift_right_trunc (a - (neg_n /< a)) 1) a

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
  q + if (z2 * x land (b - one)) + (q land one) > b then one else zero

let div_nearest a b =
  let open Z in
  let q, r = div_rem a b in
  q + if (z2 * r) + (q land one) > b then one else zero

(** [ilog ?l x m] is the integer approximation to [m * log (x / m)], with
    absolute error boundable in terms only of [x / m].

    Given positive integers [x] and [m], return an integer approximation to
    [m * log (x / m)]. For [l = 8] and [0.1 <= x / m <= 10] the difference
    between the approximation and the exact result is at most 22. For [l = 8]
    and [1.0 <= x / m <= 10.0] the difference is at most 15. In both cases
    these are upper bounds on the error; it will usually be much smaller. *)
let ilog ?(l = 8) x m =
  let l_minus r = l - !r in
  let r_minus_l r = !r - l in
  let y = ref Z.(x - m) in

  (* argument reduction; r = number of reductions performed *)
  let r = ref 0 in
  while
    (!r <= l && Z.(abs !y lsl l_minus r >= m))
    || (!r > l && Z.(abs !y asr r_minus_l r >= m))
  do
    y :=
      div_nearest
        Z.((m * !y) lsl 1)
        Z.(m + sqrt_nearest (m * (m + rshift_nearest !y !r)) m);
    incr r
  done;

  let y = !y in
  let r = !r in

  (* Taylor series with [t] terms *)
  let t = -(-10 * String.length (Z.to_string m) / 3 * l) in
  let yshift = rshift_nearest y r in
  let w = ref (div_nearest m (Z.of_int t)) in
  for k = t - 1 downto 0 do
    let zk = Z.of_int k in
    w := Z.(div_nearest m zk - div_nearest (yshift * !w) m)
  done;

  div_nearest Z.(!w * y) m

let log10_digits = ref "23025850929940456840179914546843642076011014886"

(** [log10_digits p] is [(floor 10**p) * log 10], given [p >= 0]. For example,
    [log10_digits 3] is 2302. *)
let log10_digits p =
  let return () = p + 1 |> String.sub !log10_digits 0 |> Z.of_string in
  if p < 0 then
    invalid_arg "p should be non-negative"
  else if p >= String.length !log10_digits then begin
    (* compute p+3, p+6, p+9, ... digits; continue until at least one of the
       extra digits is nonzero *)
    let extra = ref 3 in
    let continue = ref true in
    let digits = ref "" in
    while !continue do
      (* compute p+extra digits, correct to within 1ulp *)
      let m = Z.pow z10 (p + !extra + 2) in
      digits := z100 |> div_nearest (ilog Z.(z10 * m) m) |> Z.to_string;
      let check = Str.regexp (String.make !extra '0' ^ "$") in
      if Str.string_match check !digits 0 then begin
        extra := !extra + 3
      end
      else begin
        continue := false
      end
    done;
    (* keep all reliable digits so far; remove trailing zeroes and next non-zero
       digit. *)
    log10_digits := Str.replace_first unreliable "" !digits;
    return ()
  end
  else
    return ()

(** [dlog10 c e p] is an integer approximation of [10**p * log10 (c * 10**e)],
    with an absolute error of at most 1. Assumes that:

    - [c > 0]
    - [p >= 0]
    - [c * 10**e] is not exactly 1 *)
let dlog10 c e p =
  let p = p + 2 in
  let l = c |> Z.to_string |> String.length in
  let f =
    let e_plus_l = e + l in
    e_plus_l - if e_plus_l >= 1 then 1 else 0
  in
  let log_d, log_tenpower =
    if p > 0 then
      let m = Z.pow z10 p in
      let k = e + p - f in
      let c =
        if k >= 0 then
          Z.(c * pow z10 k)
        else
          div_nearest c (Z.pow z10 ~-k)
      in
      let log_d = ilog c m in
      (* error < 5 + 22 = 27 *)
      let log_10 = log10_digits p in
      (* error < 1 *)
      div_nearest Z.(log_d * m) log_10, Z.(of_int f * m)
    else
      Z.zero, div_nearest (Z.of_int f) (Z.pow z10 ~-p)
  in
  div_nearest Z.(log_tenpower + log_d) z100

(** [dlog c e p] is an integer approximation of [10**p * log (c * 10 * e)],
    with an absolute error of at most 1. Assumes that [c * 10 * e] is not
    exactly 1. *)
let dlog c e p =
  (* Increase precision by 2. The precision increase is compensated for at the
     end with a division by 100. *)
  let p = p + 2 in
  (* Rewrite [c * 10**e] as [d * 10**f] with either f >= 0 and 1 <= d <= 10, or
     f <= 0 and 0.1 <= d <= 1. Then we can compute [10**p * log (c * 10 * e)] as
     [10**p * log d + 10**p * f * log 10]. *)
  let l = c |> Z.to_string |> String.length in
  let f =
    let e_plus_l = e + l in
    e_plus_l - if e_plus_l >= 1 then 1 else 0
  in
  (* compute approximation to [10**p * log d], with error < 27 *)
  let log_d =
    if p > 0 then
      let k = e + p - f in
      let c =
        if k >= 0 then
          Z.(c * pow z10 k)
        else
          div_nearest c (Z.pow z10 ~-k)
        (* error of <= 0.5 in c *)
      in
      (* ilog magnifies existing error in [c] by a factor of at most 10 *)
      ilog c (Z.pow z10 p)
    else (* [p <= 0]: just approximate the whole thing by 0; error < 2.31 *)
      Z.zero
  in
  let extra = (f |> abs |> string_of_int |> String.length) - 1 in
  (* compute approximation to [f * 10**p*log 10], with error < 11. *)
  let f_log_ten =
    if f <> 0 then
      let p_plus_extra = p + extra in
      if p_plus_extra >= 0 then
        (* error in [f * log10_digits (p + extra) < |f| * 1 = |f| *]
           after division, [error < |f| / 10**extra + 0.5 < 10 + 0.5 < 11 *)
        div_nearest Z.(of_int f * log10_digits p_plus_extra) (Z.pow z10 extra)
      else
        Z.zero
    else
      Z.zero
  in
  (* error in sum < 11 + 27 = 38; error after division < 0.38 + 0.5 < 1 *)
  div_nearest Z.(f_log_ten + log_d) z100

(** [iexp ?l x m] is an integer approximation of [m * exp (x / m)], given
    [m > 0] and such that [x / m] is small in absolute value. For
    [0 <= x / m <= 2.4], the absolute error in the result is bounded by 60 (and
    is usually much smaller). *)
let iexp ?(l = 8) x m =
  (* Find [r] such that [x / 2**r/m <= 2 ** ~-l] *)
  let r = Z.(numbits ((x lsl l) /< m)) in

  (* Taylor series. [(2 ** l)**t > m] *)
  let t = -(-10 * String.length (Z.to_string m) / 3 * l) in
  let y = ref (div_nearest x (Z.of_int t)) in
  let mshift = ref Z.(m lsl r) in
  for i = t - 1 downto 0 do
    let mshift = !mshift in
    y := div_nearest Z.((x * mshift) + !y) Z.(mshift * of_int i)
  done;

  (* Expansion *)
  for k = r - 1 downto -1 do
    let k_plus_2 = k + 2 in
    (mshift := Z.(m lsl k_plus_2));
    let y_val = !y in
    let mshift = !mshift in
    y := div_nearest Z.(y_val * (y_val + mshift)) mshift
  done;
  Z.(m + !y)

(** [dexp c e p] is an approximation to [exp (c * 10**e)], with [p] decimal
    places of precision.

    Returns integers [d, f] such that:

    - [10**(p - 1) <= d <= 10**p], and
    - [(d - 1) * 10**f < exp (c * 10**e) < (d + 1) * 10**f]

    In other words, [d * 10**f] is an approximation to [exp (c * 10**e)] with
    [p] digits of precision, and with an error in [d] of at most [1]. This is
    almost, but not quite, the same as the error being < 1ulp: when
    [d = 10**(p - 1)] the error could be up to 10 ulp. *)
let dexp c e p =
  (* we'll call [iexp] with [m = 10**(p + 2)], giving [p + 3] digits of
     precision *)
  let p = p + 2 in

  (* compute [log 10] with extra precision = adjusted exponent of [c * 10**e] *)
  let extra = max 0 (e + String.length (Z.to_string c) - 1) in
  let q = p + extra in

  (* compute quotient [c * 10**e/(log 10) = c * 10**(e + q)/(log 10 * 10**q)],
     rounding down *)
  let shift = e + q in
  let cshift =
    if shift >= 0 then
      Z.(c * pow z10 shift)
    else
      let shift = -shift in
      Z.(c /< pow z10 shift)
  in
  let quot, rem = Z.(div_rem cshift (log10_digits q)) in

  (* reduce remainder back to original precision *)
  let rem = div_nearest rem (Z.pow z10 extra) in

  (* error in result of [iexp < 120]; error after division < 0.62 *)
  div_nearest (iexp rem (Z.pow z10 p)) (Z.of_int 1_000), Z.to_int quot - p + 3

(** [dpower xc xe yc ye p] is [x ** y], given integers [xc], [xe], [yc], and
    [ye] representing decimals [x = xc * 10**xe] and [y = yc * 10**ye]. Returns
    a pair of integers [c, e] such that:

    - [10**(p - 1) <= c <= 10**p], and
    - [(c - 1) * 10**e < x**y < (c + 1) * 10**e]

    In other words, [c * 10**e] is an approximation to [x**y] with [p] digits of
    precision, and with an error in [c] of at most [1]. This almost, but not
    quite, the same as the error being < 1ulp: when [c = 10**(p - 1)] we can
    only guarantee error < 10ulp.

    We assume that: [x] is positive and not equal to [1], and [y] is nonzero. *)
let dpower xc xe yc ye p =
  (* Find [b] such that [10**(b - 1) <= abs y <= 10**b] *)
  let b = ye + (yc |> Z.abs |> Z.to_string |> String.length) in

  (* [log x = lxc * 10**(-p - b - 1)], to [p + b + 1] places after the decimal
     point *)
  let lxc = dlog xc xe (p + b + 1) in

  (* compute product
     [y * log x = yc * lxc * 10**(-p - b - 1 + ye) = pc * 10**(-p - 1)] *)
  let shift = ye - b in
  let pc =
    if shift >= 0 then
      Z.(lxc * yc * pow z10 shift)
    else
      let shift = -shift in
      div_nearest Z.(lxc * yc) (Z.pow z10 shift)
  in
  let coef, exp =
    if Z.(equal pc zero) then
      (* we prefer a result that isn't exactly 1; this makes it easier to
         compute a correctly rounded result in [pow] *)
      if (xc |> Z.to_string |> String.length) + xe >= 1 = Z.(gt yc zero) then
        let p_minus_1 = p - 1 in
        Z.(pow z10 p_minus_1 + one), 1 - p
      else
        Z.(pow z10 p - one), -p
    else
      let coef, exp = dexp pc ~-(p + 1) (p + 1) in
      let coef = div_nearest coef z10 in
      coef, succ exp
  in
  coef, exp

(** [log10_lb ?correction c] is a lower bound for [100 * log10 c] for a positive
    integer [c]. *)
let log10_lb
    ?(correction =
      [ '1', 100;
        '2', 70;
        '3', 53;
        '4', 40;
        '5', 31;
        '6', 23;
        '7', 16;
        '8', 10;
        '9', 5 ]) c =
  if Z.(Compare.(c <= zero)) then
    invalid_arg "log10_lb: argument should be non-negative"
  else
    let str_c = Z.to_string c in
    (100 * String.length str_c) - List.assoc str_c.[0] correction
