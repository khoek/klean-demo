import system.extras
import .lib

open extras

def dim : ℕ := 2
def vects : list (array dim ℕ) := [to_array [1, 2], to_array [4, 5]]

meta def main : io unit := do
    io.print_ln "a",
    extras.greet,
    n ← pure (extras.find_separating_hyperplane vects),
    io.print_ln "b",
    io.print_ln n