import system.extras
import .lib

def dim : ℕ := 2
def a_vects : list (array dim ℕ) := [to_array [1, 2], to_array [4, 5]]
def b_vects : list (array dim ℕ) := [to_array [5, 5], to_array [7, 8]]

meta def main : io unit := do
    io.print_ln "a",
    extras.greet,
    n ← pure (extras.find_separating_hyperplane a_vects b_vects),
    io.print_ln "b",
    io.print_ln n