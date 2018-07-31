import system.io

open string

-- ################# Array manipulation ###################

def to_array_internal {N : ℕ} {α : Type}
    : array N α → list α → ℕ → array N α
    | ar [] n := ar
    | ar (a :: l) n :=
        or.by_cases (lt_or_ge n N)
            (λ h, to_array_internal (array.write ar ⟨n, h⟩ a) l (n + 1))
            (λ h, ar) -- unreachable, if to_array_internal was called by to_array

-- FIXME implement a native version of this: 1. for speed, and 2. because it is crazy
-- that α has to be inhabited!
def to_array {α : Type} [inhabited α] (l : list α) : array (list.length l) α :=
    let ar := mk_array (list.length l) (default α) in
    to_array_internal ar l 0

-- ################# String editing ####################

-- FIXME the corelib definitions of iterator.prev_to_string and 
-- iterator.next_to_string are horrendously slow

-- FIXME no error handling
meta def iterator_advance : iterator → ℕ → iterator
    | it 0 := it
    | it n := iterator_advance (iterator.next it) (n - 1)

-- FIXME no error handling
meta def iterator_receed : iterator → ℕ → iterator
    | it 0 := it
    | it n := iterator_receed (iterator.prev it) (n - 1)

meta def substring_front_internal (it : iterator) (s : ℕ) : string :=
    iterator.next_to_string (iterator_advance it s)

-- FIXME no error handling
meta def substring_front (str : string) (s : ℕ) : string :=
    substring_front_internal (mk_iterator str) s

meta def substring_back_internal (it : iterator) (e : ℕ) : string :=
    iterator.prev_to_string (iterator_receed it e)

-- FIXME no error handling
meta def substring_back (str : string) (e : ℕ) : string :=
    substring_back_internal (iterator.to_end (mk_iterator str)) e

-- FIXME no error handling
meta def substring_internal : iterator → ℕ → ℕ → string := 
-- FIXME I don't know why the lambdas have to be here (instead of chunking them in
-- the first part of the declaration), but they do! I don't understand the
-- difference, which is something I should learn. 
    λ it, λ s, λ e,
    if s > e then
        ""
    else if s > 0 then
        let new_it := mk_iterator (substring_front_internal it s) in
        substring_internal new_it 0 (e - s + 1)
    else
        iterator.prev_to_string (iterator_advance it e)

-- apparently substring isn't declared in the corelib :O
-- I don't know what else people are doing with strings!
meta def substring (str : string) (s e : ℕ) : string :=
    substring_internal (mk_iterator str) s e

-- FIXME error handling
meta def string_cut (str : string) (pos : ℕ) : string × char × string :=
    let it := iterator_advance (mk_iterator str) pos in
    (iterator.prev_to_string it, iterator.curr it, iterator.next_to_string it)



-- ########### String searching functions ################

def noninsane_next_internal (it : iterator) : bool → option (iterator × char)
    | ff := none
    | tt := some (iterator.next it, iterator.curr it)

/- iterator.curr is absolute nonsense: default char = 'A'! PPPPPPCCCHHHHHWWWWW *head explodes* -/
-- advance the iterator and return the popped char, or nothing if there is nothing.....
def noninsane_next (it : iterator) : option (iterator × char) :=
    noninsane_next_internal it (iterator.has_next it)

def add_one : nat → option ℕ := λ n, some (n + 1)

meta def find_char_internal (pattern : char) : option (iterator × char) → option nat
    | none := none
    | (some (it, c)) :=
        if c = pattern then
            some 1
        else
            option.cases_on (find_char_internal (noninsane_next it))
                none
                (λ n, some (n + 1))

meta def find_char (pattern : char) (str : string) : option nat :=
    find_char_internal pattern (noninsane_next (string.mk_iterator str))



-- ############ Networking ############

def read_sock (sock : io.socket) (n : ℕ) : io string := do
    str ← io.net.recv sock n,
    return str^.to_string