# bankopak

Compress and decompress banko boards.

Run `make` to build.

To compress a series of banko boards, run

    $ ./bankopak -c ALGORITHM < path/to/banko_boards.bankopladeformat > banko_boards.bankopak

where `ALGORITHM` is a number between 0 and whatever the highest algorithm id
is.

To decompress, run

    $ ./bankopak -d ALGORITHM < path/to/banko_boards.bankopak > banko_boards.bankopladeformat

Algorithm 8 requires a Rust installation; see https://rust-lang.org/ for
help.
