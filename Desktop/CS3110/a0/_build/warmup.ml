(** @author Morgan Thalia Aloia (mta47) *)

(** valid_date y m d is a boolean checker which is true 
if y m d is a Gregorian calendar date and is false otherwise.*)
let valid_date y m d = 
  if y >= 1
  then
    if List.mem m ["Jan"; "Mar"; "May"; "Jul"; "Aug"; "Oct"; "Dec"]
    then 
      if d >= 1 && d <= 31
      then true
      else false
    else
    if List.mem m ["Apr"; "Jun"; "Sep"; "Nov";]
    then 
      if d >= 1 && d <= 30
      then true
      else false
    else
    if m = "Feb"
    then 
      if (y mod 4 = 0 && y mod 100 != 0) || (y mod 400 = 0)
      then
        if d >= 1 && d <= 29
        then true
        else false
      else
        if d >= 1 && d <= 28
        then true
        else false
    else false
  else false 



(** syr n is the number of Collatz opperations required for Int n to equal 1.
Requires: n > 0*)
let rec syr ?c:(c=0) n = 
  if n = 1
  then c
  else
    if n mod 2 = 0
    then syr ~c:(c + 1) (n / 2)
    else syr ~c:(c + 1) (3 * n + 1)



(** list_end lis is the final element of List lis *)
let list_end lis = List.hd (List.rev lis)

(** rev_tail lis is the final element of List lis *)
let rev_tail lis = List.rev (List.tl (List.rev lis))

(** sum_last_n lis n is the sum of the final Int n elements of List lis. 
The value of a negative position in List lis = 0.
Requires: n > 0*)
let rec sum_last_n ?c:(c=0) lis n = 
  if n = 0 || List.length lis = 0
  then c
  else sum_last_n ~c:(c + list_end lis) (rev_tail lis) (n - 1)

(** nacci n k is a List of the first Int k elements of 
the Int n-step Fibonacci sequence
Requires: n,k > 0*)
let rec nacci ?lis:(lis=[]) ?i:(i=0) n k = 
  if i = k
  then lis
  else
    if i = 0 || i = 1
    then nacci ~lis:(lis @ [1]) ~i:(i + 1) n k
    else nacci ~lis:(lis @ [sum_last_n lis n]) ~i:(i + 1) n k
  



(* valid_date assertions *)
  (* Test valid non-leap day. *)
  let () = assert (valid_date 2019 "Dec" 31)

  (* Test invalid leap day. *)
  let () = assert (not (valid_date 2019 "Feb" 29))

  (* Test quatricentenial leap day. *)
  let () = assert (valid_date 1600 "Feb" 29)

(* syr assertions *)
  (* Test init *)
  let () = assert (syr 1 = 0)

  (* Test starting even *)
  let () = assert (syr 6 = 8)

  (* Test starting odd *)
  let () = assert (syr 13 = 9)

(* nacci assertions *)
  (* Test n < k *)
  let () = assert (nacci 2 5 = [1; 1; 2; 3; 5])

  (* Test n > k *)
  let () = assert (nacci 10 5 = [1; 1; 2; 4; 8])

  (* Test n = k *)
  let () = assert (nacci 5 5 = [1; 1; 2; 4; 8])

let hours_worked = 4