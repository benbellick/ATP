type prop = P of string

val eval : 'a Formula_core.formula -> ('a -> bool) -> bool
val pname : prop -> string
val parse_propvar : 'a -> string list -> prop Formula_core.formula * string list
val parse_prop_formula : CCString.t -> prop Formula_core.formula
val default_parser : CCString.t -> prop Formula_core.formula
val print_propvar : 'a -> prop -> unit
val pp_propvar : CCFormat.t -> prop -> unit
val print_prop_formula : prop Formula_core.formula -> unit

val pp_prop_formula : CCFormat.formatter -> prop Formula_core.formula -> unit
[@@ocaml.toplevel_printer]

val onallvaluations : (('a -> bool) -> bool) -> ('a -> bool) -> 'a list -> bool
val to_string : prop Formula_core.formula -> string
val print_truthtable : prop Formula_core.formula -> unit
val tautology : 'a Formula_core.formula -> bool
val unsatisfiable : 'a Formula_core.formula -> bool
val satisfiable : 'a Formula_core.formula -> bool

val psubst :
  ('a, 'a Formula_core.formula) Fpf.func ->
  'a Formula_core.formula ->
  'a Formula_core.formula

val dual : 'a Formula_core.formula -> 'a Formula_core.formula
val psimplify1 : 'a Formula_core.formula -> 'a Formula_core.formula
val psimplify : 'a Formula_core.formula -> 'a Formula_core.formula
val negative : 'a Formula_core.formula -> bool
val positive : 'a Formula_core.formula -> bool
val negate : 'a Formula_core.formula -> 'a Formula_core.formula
val nnf : 'a Formula_core.formula -> 'a Formula_core.formula
val nenf : 'a Formula_core.formula -> 'a Formula_core.formula
val list_conj : 'a Formula_core.formula list -> 'a Formula_core.formula
val list_disj : 'a Formula_core.formula list -> 'a Formula_core.formula

val mk_lits :
  'a Formula_core.formula CCList.t -> ('a -> bool) -> 'a Formula_core.formula

val allsatvaluations :
  (('a -> bool) -> bool) -> ('a -> bool) -> 'a list -> ('a -> bool) list

val _dnf : 'a Formula_core.formula -> 'a Formula_core.formula
val rawdnf : 'a Formula_core.formula -> 'a Formula_core.formula
val allpairs : ('a -> 'b -> 'c) -> 'a CCList.t -> 'b CCList.t -> 'c CCList.t
val distrib : 'a CCList.t CCList.t -> 'a CCList.t CCList.t -> 'a CCList.t list

val purednf :
  'a Formula_core.formula -> 'a Formula_core.formula CCList.t CCList.t

val trivial : 'a Formula_core.formula list -> bool

val simpdnf :
  'a Formula_core.formula -> 'a Formula_core.formula CCList.t CCList.t

val dnf : 'a Formula_core.formula -> 'a Formula_core.formula
val purecnf : 'a Formula_core.formula -> 'a Formula_core.formula list list

val simpcnf :
  'a Formula_core.formula -> 'a Formula_core.formula CCList.t CCList.t

val cnf : 'a Formula_core.formula -> 'a Formula_core.formula

module Examples : sig
  val ramsey : int -> int -> int -> prop Formula_core.formula

  val halfsum :
    'a Formula_core.formula ->
    'a Formula_core.formula ->
    'a Formula_core.formula

  val halfcarry :
    'a Formula_core.formula ->
    'a Formula_core.formula ->
    'a Formula_core.formula

  val ha :
    'a Formula_core.formula ->
    'a Formula_core.formula ->
    'a Formula_core.formula ->
    'a Formula_core.formula ->
    'a Formula_core.formula

  val carry :
    'a Formula_core.formula ->
    'a Formula_core.formula ->
    'a Formula_core.formula ->
    'a Formula_core.formula

  val sum :
    'a Formula_core.formula ->
    'a Formula_core.formula ->
    'a Formula_core.formula ->
    'a Formula_core.formula

  val fa :
    'a Formula_core.formula ->
    'a Formula_core.formula ->
    'a Formula_core.formula ->
    'a Formula_core.formula ->
    'a Formula_core.formula ->
    'a Formula_core.formula

  val conjoin :
    ('a -> 'b Formula_core.formula) -> 'a CCList.t -> 'b Formula_core.formula

  val ripplecarry :
    (int -> 'a Formula_core.formula) ->
    (int -> 'a Formula_core.formula) ->
    (int -> 'a Formula_core.formula) ->
    (int -> 'a Formula_core.formula) ->
    int ->
    'a Formula_core.formula

  val mk_index : string -> int -> prop Formula_core.formula
  val mk_index2 : string -> int -> int -> prop Formula_core.formula

  val ripplecarry0 :
    (int -> 'a Formula_core.formula) ->
    (int -> 'a Formula_core.formula) ->
    (int -> 'a Formula_core.formula) ->
    (int -> 'a Formula_core.formula) ->
    int ->
    'a Formula_core.formula

  val ripplecarry1 :
    (int -> 'a Formula_core.formula) ->
    (int -> 'a Formula_core.formula) ->
    (int -> 'a Formula_core.formula) ->
    (int -> 'a Formula_core.formula) ->
    int ->
    'a Formula_core.formula

  val mux :
    'a Formula_core.formula ->
    'a Formula_core.formula ->
    'a Formula_core.formula ->
    'a Formula_core.formula

  val offset : int -> (int -> 'a) -> int -> 'a

  val carryselect :
    (int -> 'a Formula_core.formula) ->
    (int -> 'a Formula_core.formula) ->
    (int -> 'a Formula_core.formula) ->
    (int -> 'a Formula_core.formula) ->
    (int -> 'a Formula_core.formula) ->
    (int -> 'a Formula_core.formula) ->
    (int -> 'a Formula_core.formula) ->
    (int -> 'a Formula_core.formula) ->
    int ->
    int ->
    'a Formula_core.formula

  val mk_adder_test : int -> int -> prop Formula_core.formula

  val rippleshift :
    (int -> 'a Formula_core.formula) ->
    (int -> 'a Formula_core.formula) ->
    (int -> 'a Formula_core.formula) ->
    'a Formula_core.formula ->
    (int -> 'a Formula_core.formula) ->
    int ->
    'a Formula_core.formula

  val multiplier :
    (int -> int -> 'a Formula_core.formula) ->
    (int -> int -> 'a Formula_core.formula) ->
    (int -> int -> 'a Formula_core.formula) ->
    (int -> 'a Formula_core.formula) ->
    int ->
    'a Formula_core.formula

  val bitlength : int -> int
  val bit : int -> int -> bool

  val congruent_to :
    (int -> 'a Formula_core.formula) -> int -> int -> 'a Formula_core.formula

  val prime : int -> prop Formula_core.formula
end

val mkprop : int -> prop Formula_core.formula * int

val maincnf :
  prop Formula_core.formula
  * ( prop Formula_core.formula,
      prop Formula_core.formula * prop Formula_core.formula )
    Fpf.func
  * int ->
  prop Formula_core.formula
  * ( prop Formula_core.formula,
      prop Formula_core.formula * prop Formula_core.formula )
    Fpf.func
  * int

val defstep :
  (prop Formula_core.formula ->
  prop Formula_core.formula ->
  prop Formula_core.formula) ->
  prop Formula_core.formula * prop Formula_core.formula ->
  prop Formula_core.formula
  * ( prop Formula_core.formula,
      prop Formula_core.formula * prop Formula_core.formula )
    Fpf.func
  * int ->
  prop Formula_core.formula
  * ( prop Formula_core.formula,
      prop Formula_core.formula * prop Formula_core.formula )
    Fpf.func
  * int

val max_varindex : string -> string -> int -> int

val mk_defcnf :
  (prop Formula_core.formula * ('a, 'b) Fpf.func * int ->
  'c Formula_core.formula * ('d, 'e * 'c Formula_core.formula) Fpf.func * 'f) ->
  prop Formula_core.formula ->
  'c Formula_core.formula CCList.t list

val defcnf_old : prop Formula_core.formula -> prop Formula_core.formula

val subcnf :
  ('a Formula_core.formula * 'b * 'c -> 'd * 'b * 'c) ->
  ('d -> 'd -> 'e) ->
  'a Formula_core.formula * 'a Formula_core.formula ->
  'f * 'b * 'c ->
  'e * 'b * 'c

val orcnf :
  prop Formula_core.formula
  * ( prop Formula_core.formula,
      prop Formula_core.formula * prop Formula_core.formula )
    Fpf.func
  * int ->
  prop Formula_core.formula
  * ( prop Formula_core.formula,
      prop Formula_core.formula * prop Formula_core.formula )
    Fpf.func
  * int

val andcnf :
  prop Formula_core.formula
  * ( prop Formula_core.formula,
      prop Formula_core.formula * prop Formula_core.formula )
    Fpf.func
  * int ->
  prop Formula_core.formula
  * ( prop Formula_core.formula,
      prop Formula_core.formula * prop Formula_core.formula )
    Fpf.func
  * int

val defcnfs :
  prop Formula_core.formula -> prop Formula_core.formula CCList.t list

val defcnf : prop Formula_core.formula -> prop Formula_core.formula

val andcnf3 :
  prop Formula_core.formula
  * ( prop Formula_core.formula,
      prop Formula_core.formula * prop Formula_core.formula )
    Fpf.func
  * int ->
  prop Formula_core.formula
  * ( prop Formula_core.formula,
      prop Formula_core.formula * prop Formula_core.formula )
    Fpf.func
  * int

val defcnf3 : prop Formula_core.formula -> prop Formula_core.formula

val one_literal_rule :
  'a Formula_core.formula CCList.t CCList.t -> 'a Formula_core.formula list list

val affirmative_negative_rule :
  'a Formula_core.formula CCList.t CCList.t ->
  'a Formula_core.formula CCList.t CCList.t

val resolve_on :
  'a Formula_core.formula ->
  'a Formula_core.formula CCList.t list ->
  'a Formula_core.formula CCList.t CCList.t

val resolution_blowup :
  'a Formula_core.formula CCList.t CCList.t -> 'a Formula_core.formula -> int

val resolution_rule :
  'a Formula_core.formula CCList.t CCList.t ->
  'a Formula_core.formula CCList.t CCList.t

val dp : 'a Formula_core.formula CCList.t CCList.t -> bool
val dpsat : prop Formula_core.formula -> bool
val dptaut : prop Formula_core.formula -> bool

val posneg_count :
  'a Formula_core.formula CCList.t CCList.t -> 'a Formula_core.formula -> int

val dpll : 'a Formula_core.formula CCList.t CCList.t -> bool
val dpllsat : prop Formula_core.formula -> bool
val dplltaut : prop Formula_core.formula -> bool

type trailmix = Guessed | Deduced

val unassigned :
  'a Formula_core.formula CCList.t CCList.t ->
  ('a Formula_core.formula * 'b) CCList.t ->
  'a Formula_core.formula list

val unit_subpropagate :
  'a Formula_core.formula CCList.t CCList.t
  * ('a Formula_core.formula, unit) Fpf.func
  * ('a Formula_core.formula * trailmix) list ->
  'a Formula_core.formula CCList.t CCList.t
  * ('a Formula_core.formula, unit) Fpf.func
  * ('a Formula_core.formula * trailmix) list

val unit_propagate :
  'a Formula_core.formula CCList.t CCList.t
  * ('a Formula_core.formula * trailmix) CCList.t ->
  'a Formula_core.formula CCList.t CCList.t
  * ('a Formula_core.formula * trailmix) CCList.t

val backtrack : ('a * trailmix) list -> ('a * trailmix) list

val dpli :
  'a Formula_core.formula CCList.t CCList.t ->
  ('a Formula_core.formula * trailmix) CCList.t ->
  bool

val dplisat : prop Formula_core.formula -> bool
val dplitaut : prop Formula_core.formula -> bool

val backjump :
  'a Formula_core.formula CCList.t CCList.t ->
  'a Formula_core.formula ->
  ('a Formula_core.formula * trailmix) list ->
  ('a Formula_core.formula * trailmix) list

val dplb :
  'a Formula_core.formula CCList.t CCList.t ->
  ('a Formula_core.formula * trailmix) CCList.t ->
  bool

val dplbsat : prop Formula_core.formula -> bool
val dplbtaut : prop Formula_core.formula -> bool

val triplicate :
  prop Formula_core.formula ->
  prop Formula_core.formula * prop Formula_core.formula CCList.t
