-- Massively parallel Monte Carlo simulation of Banko!

type num = i8 -- Enough for 1-90.

type row = (num, num, num, num, num)
type board = (row, row, row)

let contains (b: board) (c: num) =
  let row_contains (r: row) =
    r.1 == c || r.2 == c || r.3 == c || r.4 == c || r.5 == c
  in row_contains b.1 || row_contains b.2 || row_contains b.3

import "/futlib/random"

module mk_ryst_posen (E: rng_engine): {
  val ryst_posen [n]: E.rng -> (p: [n]num) -> (E.rng, [n]num)
} = {

module rng = uniform_int_distribution i32 E

-- Fisher-Yates shuffle.
let ryst_posen [n] (r: E.rng) (p: [n]num) =
  loop (r, p) = (r, copy p) for i in n-1..n-2...1 do unsafe
    let (r, j) = rng.rand (0, i) r
    let elem_j = p[j]
    let p[j] = p[i]
    let p[i] = elem_j
    in (r, p)
}

module ryst = mk_ryst_posen minstd_rand

let board_from_array (board: [3][5]num): board =
  let row r = (r[0], r[1], r[2], r[3], r[4])
  in (row board[0], row board[1], row board[2])

let turns_to_win (picks: []num) (board: board) =
  (loop (remaining, i) = (15, 0) while remaining > 0 && i < length picks do
     if unsafe board `contains` picks[i]
     then (remaining - 1, i+1)
     else (remaining,     i+1)).2

let main [num_boards] (simultaneous_games: i32) (boards: [num_boards][3][5]num) =
  let rngs = [num_boards*simultaneous_games]
             |> minstd_rand.rng_from_seed
             |> minstd_rand.split_rng simultaneous_games
  let (_, paths) = map2 ryst.ryst_posen rngs (replicate simultaneous_games (1...90)) |> unzip2
  let boards = map board_from_array boards
  in paths
     |> map (\picks -> map (turns_to_win picks) boards)
