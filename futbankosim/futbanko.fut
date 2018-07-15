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

module rng_engine = xorshift128plus
module ryst = mk_ryst_posen rng_engine

let board_from_array (board: [3][5]num): board =
  let row r = (r[0], r[1], r[2], r[3], r[4])
  in (row board[0], row board[1], row board[2])

type winnage = {one_row: i32, two_rows: i32, three_rows: i32}

let turns_to_win (picks: []num) (board: board): winnage =
  (loop (remaining, i, {one_row, two_rows, three_rows}) =
        (15, 0, {one_row=0, two_rows=0, three_rows=0})
   while remaining > 0 && i < length picks do
     if unsafe board `contains` picks[i]
     then (remaining - 1, i+1,
           {one_row = if remaining == 11 then i+1 else one_row,
            two_rows = if remaining == 6 then i+1 else two_rows,
            three_rows = if remaining == 1 then i+1 else three_rows})
     else (remaining, i+1, {one_row, two_rows, three_rows})).3

type winner = {who: i32, len: i32 }
type winners = {one_row:    winner,
                two_rows:   winner,
                three_rows: winner}

let find_winners [num_boards] (boards: [num_boards]winnage): winners =
  let row_winner (w1: winner) (w2: winner) =
    if      w1.len < w2.len then w1
    else if w2.len < w1.len then w2
    else if w1.who < w2.who then w1
    else                         w2

  let game_winner (w1: winners) (w2: winners) =
    {one_row = row_winner w1.one_row w2.one_row,
     two_rows = row_winner w1.two_rows w2.two_rows,
     three_rows = row_winner w1.three_rows w2.three_rows}

  let no_winner = {one_row    = {who= -1, len=999},
                   two_rows   = {who= -1, len=999},
                   three_rows = {who= -1, len=999}}

  let winnage_to_winner who ({one_row, two_rows, three_rows}: winnage): winners =
    {one_row = {who, len = one_row},
     two_rows = {who, len = two_rows},
     three_rows = {who, len = three_rows}}

  in boards
     |> map2 winnage_to_winner (iota num_boards)
     |> reduce game_winner no_winner

let run_game (boards: []board) (picks: []num): winners =
  boards
  |> map (turns_to_win picks)
  |> find_winners

type game_winners = []winners

-- | Main simulation entry point.
entry run [num_boards] (seed: i32)
                       (simultaneous_games: i32)
                       (boards: [num_boards][3][5]num)
                     : game_winners =
  let rngs = [seed ^ num_boards ^ simultaneous_games]
             |> rng_engine.rng_from_seed
             |> rng_engine.split_rng simultaneous_games
  let (_, paths) = replicate simultaneous_games (1...90)
                   |> map2 ryst.ryst_posen rngs
                   |> unzip2
  let boards = map board_from_array boards
  in map (run_game boards) paths

-- | Extracting arrays of, for each game, the index of the winning
-- board for one row, two rows, and three rows.
entry winners_per_game (ws: game_winners): ([]i32, []i32, []i32) =
  (map (.one_row.who) ws,
   map (.two_rows.who) ws,
   map (.three_rows.who) ws)
