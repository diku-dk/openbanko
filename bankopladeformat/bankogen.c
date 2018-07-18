/*
 * Generate random banko boards with the help of /dev/urandom.
 */

#include <stdlib.h>
#include <stdio.h>
#include <error.h>
#include <errno.h>
#include <string.h>
#include <getopt.h>
#include <stdint.h>

#include "bankopladeformat.h"

int32_t masks[1<<BANKO_MASK_INDEX_BITS] = { 0 };
int32_t mask_to_index[1<<BANKO_MASK_BITS] = { -1 };

static int read_mask_index(FILE *in) {
  while (1) {
    uint32_t x;
    if (fread(&x, 3, 1, in) != 1) {
      return 0;
    }
    x >>= 8;

    if (x < BANKO_NUM_MASKS) {
      return x;
    }
  }
}

// Only works if the span is less than 256.
static int read_number(FILE *in, int min, int max) {
  int span = max - min + 1;
  int fits = 256 / span;

  while (1) {
    uint8_t b;
    if (fread(&b, 1, 1, in) != 1) {
      return 0;
    }

    if (b >= fits * span) {
      continue;
    } else {
      return min + (b % span);
    }
  }
}

static void gen_boards(struct banko_writer *writer, int nboards) {
  FILE* in = fopen("/dev/urandom", "rb");
  struct board b;

  for (int k = 0; k < nboards; k++) {
    int mask = masks[read_mask_index(in)];

    for (int j = 0; j < BOARD_COLS; j++) {
      int min = (j == 0);

      int numbers_in_col =
        banko_lookup_mask(mask, 0, j) +
        banko_lookup_mask(mask, 1, j) +
        banko_lookup_mask(mask, 2, j);

      for (int i = 0; i < BOARD_ROWS; i++) {
        int max = (j == 0) + 9 + (j == BOARD_COLS-1);

        max -= (numbers_in_col-1);

        if (banko_lookup_mask(mask, i, j)) {
          int r = read_number(in, min, max);;
          b.cells[i][j] = r + (j * 10);
          min = r + 1;
          numbers_in_col--;
        } else {
          b.cells[i][j] = 0;
        }
      }
    }

    banko_writer_board(writer, &b);
  }

  fclose(in);
}

int main(int argc, char **argv) {
  int nboards = 1;
  int opt;

  while ((opt = getopt(argc, argv, "n:")) != -1) {
    switch (opt) {
    case 'n':
      nboards = atoi(optarg);
      break;
    default:
      fprintf(stderr, "Usage: %s -n <num_boards>\n",
              argv[0]);
      exit(EXIT_FAILURE);
    }
  }

  banko_calculate_masks(masks, mask_to_index);

  struct banko_writer writer;
  banko_writer_open(&writer, stdout);

  gen_boards(&writer, nboards);

  banko_writer_close(&writer);
}
