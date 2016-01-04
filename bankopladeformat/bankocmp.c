/*
 * cmp(1), but for .bankopladeformat-files.  Only loads two boards at
 * a time, in order to support data sets vastly larger than main
 * memory.
 */

#include <stdlib.h>
#include <stdio.h>
#include <error.h>
#include <errno.h>

#include "bankopladeformat.h"

int main(int argc, char** argv) {
  if (argc != 3) {
    fprintf(stderr, "Usage: %s file1 file2\n", argv[0]);
    exit(EXIT_FAILURE);
  }

  const char *n1 = argv[1], *n2 = argv[2];
  FILE *f1, *f2;
  struct banko_reader r1, r2;

  if ((f1 = fopen(n1, "r")) == NULL) {
    error(1, errno, "Could not open %s", n1);
  }
  if ((f2 = fopen(n2, "r")) == NULL) {
    error(1, errno, "Could not open %s", n2);
  }

  banko_reader_open(&r1, f1);
  banko_reader_open(&r2, f2);

  int board = 0;

  while (1) {
    struct board b1, b2;
    if (banko_reader_board(&r1, &b1) != 0) {
      banko_reader_clear_error(&r1);
      if (banko_reader_close(&r1) == 0) {
        if (banko_reader_close(&r2) == 0) {
          break;
        } else {
          error(1, 0, "%s ends after %d board(s), but %s continues.", n1, board, n2);
        }
      } else {
        error(1, 0, "Parse error when reading board %d from %s", board, n1);
      }
    }

    if (banko_reader_board(&r2, &b2) != 0) {
      banko_reader_clear_error(&r2);
      if (banko_reader_close(&r2) == 0) {
        if (banko_reader_close(&r1) == 0) {
          break;
        } else {
          error(1, 0, "%s ends after %d board(s), but %s continues.", n2, board, n1);
        }
      } else {
        error(1, 0, "Parse error when reading board %d from %s", board, n2);
      }
    }

    for (int col = 0; col < BOARD_COLS; col++) {
      for (int row = 0; row < BOARD_ROWS; row++) {
        uint8_t c1 = b1.cells[row][col];
        uint8_t c2 = b2.cells[row][col];
        if (c1 != c2) {
          error(1, 0, "%s and %s differ at board %d, column %d, row %d: %d != %d",
                n1, n2, board, col, row, c1, c2);
        }
      }
    }

    board++;
  }

  return 0;
}
