import system.io
import .lib

open io

def read_sock_chunk (sock : socket) : io string := do
    c ← read_sock sock 1,
    return c

meta def process_command (cmd : string) : io unit := do
-- FIXME how do you concatenate strings!?! :O
    io.print "printing:",
    io.print_ln cmd

-- The option ℕ is where the first nl character is.
-- Our job is to call process_command if we find a nl and then advance the buffer,
-- else just keep the buff how it is. We keep looking for newlines until there aren't
-- any more.
meta def parse_chunk_internal : string → option ℕ → io string
    | buff none := pure buff
    | buff (some n) := do
        (cmd, c, rest) ← pure (string_cut buff n),
        process_command cmd,
-- FIXME how do you forward declare?
        let nl_pos := find_char '\n' rest in do
        parse_chunk_internal rest nl_pos

meta def parse_chunk (buff : string) : io string :=
    let nl_pos := find_char '\n' buff in
    parse_chunk_internal buff nl_pos

-- HEY Scott: does lean implement tail call elimination? If it doesn't that means
-- its impossible to implement a request-handling loop which can run forever
-- (eventually the stack will overflow).
meta def main_loop_internal (sock : socket) : io unit := do
    io.print_ln "write",
    write_sock sock "ello\n",
    io.print_ln "read",
    read_sock sock 1,
    main_loop_internal

meta def run_main_loop (sock : socket) : io unit :=
    main_loop_internal sock

meta def main : io unit := do
    sock ← io.mk_socket_handle "/home/khoek/lsocket1",
    run_main_loop sock
