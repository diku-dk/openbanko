Tools for working with collections of banko boards.  Short summaries
of the tools in this folder:

# `bankopladeformat`

Reads a sequence of banko boards from standard input and writes them
on standard output.  Stops at the first invalid first.  A
mostly-useless test program.

# `bankocmp`

Checks whether two files contain the same sequence of banko boards.

# `bankosplit`

Interprets standard input as a sequence of banko boards and splits it
into new collections written to files given as command line options.
As an example,

    ./bankosplit 17 bankosplit_test_17 0 bankosplit_test_0 100 bankosplit_test_100 bankosplit_test_rest

will split its input into four files, containing respectively 17, 0,
100, and the remaining boards.

# `bankoanalyse`

Perform a statistical analysis of the deltas between board values.
Mostly useful for devicing heuristic compression algorithms for board
streams.
