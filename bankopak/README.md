# bankopak

Compress and decompress banko boards.

Run `make` to build.  Algorithm 8 requires a Rust installation; see
https://www.rust-lang.org/ for help.  If you do not wish to build this
algorithm, run `DISABLE_RUST=1 make` instead.

To compress a series of banko boards, run

    $ ./bankopak -c ALGORITHM < path/to/banko_boards.bankopladeformat > banko_boards.bankopak

where `ALGORITHM` is a number between 0 and whatever the highest algorithm id
is.

To decompress, run

    $ ./bankopak -d ALGORITHM < path/to/banko_boards.bankopak > banko_boards.bankopladeformat
