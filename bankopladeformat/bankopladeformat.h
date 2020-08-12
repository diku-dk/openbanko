#ifndef BANKOPLADEFORMAT_H
#define BANKOPLADEFORMAT_H

#include <stdio.h>
#include <stdint.h>
#include <ctype.h>
#include <stdlib.h>

/*
 * Library interface.
 */

#define BOARD_ROWS 3
#define BOARD_COLS 9

struct board {
  uint8_t cells[BOARD_ROWS][BOARD_COLS];
};

struct banko_writer {
  uint8_t first;
  FILE *file;
};

/* These functions return no error code. */
static void banko_writer_open(struct banko_writer *writer, FILE *file);
static void banko_writer_board(struct banko_writer *writer, const struct board *board);
static void banko_writer_close(struct banko_writer *writer);

struct banko_reader {
  uint8_t first;
  FILE *file;
  uint8_t error;
};

/* These all return non-zero on error. */
static int banko_reader_open(struct banko_reader *reader, FILE *file);
static int banko_reader_board(struct banko_reader *reader, struct board *board);
static int banko_reader_close(struct banko_reader *reader);

/* Clear the error state (when is this useful?). */
static void banko_reader_clear_error(struct banko_reader *reader);

/* More advanced functions built with the above. */
static void banko_write_all_boards(FILE *file, struct board* boards, int nboards);
static int banko_read_all_boards(FILE *file, struct board** boards, int *nboards);

#define A7_MASK_INDEX_BITS 20
#define A7_MASK_BITS 27

/* Binary mask stored in least significant 27 bits. */
static int banko_board_mask(const struct board* board);

/* Cache mask tables. */
static void banko_calculate_masks(int32_t masks[1<<A7_MASK_INDEX_BITS],
                                  int32_t mask_to_index[1<<A7_MASK_BITS]);

static int banko_lookup_mask(int32_t mask, int row, int column);

/*
 * All function definitions inlined and defined statically.
 */

static void banko_writer_open(struct banko_writer *writer, FILE *file) {
  writer->file = file;
  writer->first = 1;
}

static void banko_writer_board(struct banko_writer *writer, const struct board *board) {
  for (int row = 0; row < BOARD_ROWS; row++) {
    for (int col = 0; col < BOARD_COLS; col++) {
      fprintf(writer->file, "%.2d", board->cells[row][col]);
      if (col != BOARD_COLS-1) {
        fputc(' ', writer->file);
      }
    }
    fputc('\n', writer->file);
  }
  fputc('\n', writer->file);
}

static void banko_writer_close(struct banko_writer *writer) {
  (void)writer;
  return;
}

static void expect_char(struct banko_reader *reader, char expects) {
  if (reader->error != 0) {
    return;
  }

  char got = fgetc(reader->file);

  if (got == expects) {
    return;
  } else {
    ungetc(got, reader->file);
    reader->error = 1;
  }
}

static int expect_digits(struct banko_reader *reader) {
  if (reader->error != 0) {
    return 0;
  }

  int x;

  if (fscanf(reader->file, "%d", &x) != 1 || x < 0 || x > 90) {
    reader->error = 1;
  }
  return x;
}

static int banko_reader_open(struct banko_reader *reader, FILE *file) {
  reader->file = file;
  reader->error = 0;
  reader->first = 1;

  return reader->error;
}

static int banko_reader_board(struct banko_reader *reader, struct board *board) {
  /* Read row-by-row. */
  for (int row = 0; row < BOARD_ROWS; row++) {
    for (int col = 0; col < BOARD_COLS; col++) {
      if (col > 0) {
        expect_char(reader, ' ');
      }
      board->cells[row][col] = expect_digits(reader);
    }
    expect_char(reader, '\n');
  }
  expect_char(reader, '\n');

  return reader->error;
}

static int banko_reader_close(struct banko_reader *reader) {
  return reader->error;
}

static void banko_reader_clear_error(struct banko_reader *reader) {
  reader->error = 0;
}

static void banko_write_all_boards(FILE *file, struct board* boards, int nboards) {
  struct banko_writer writer;

  banko_writer_open(&writer, file);

  for (int i = 0; i < nboards; i++) {
    banko_writer_board(&writer, &boards[i]);
  }

  banko_writer_close(&writer);
}

static int banko_read_all_boards(FILE *in, struct board **boards_out, int *nboards) {
  struct board board;
  struct banko_reader reader;
  banko_reader_open(&reader, in);
  int capacity = 100, used = 0, error = 0;

  struct board *boards = malloc(capacity * sizeof(struct board));

  while (banko_reader_board(&reader, &board) == 0) {
    boards[used++] = board;

    if (used == capacity) {
      capacity *= 2;
      boards = realloc(boards, capacity * sizeof(struct board));
    }
  }

  banko_reader_close(&reader);

  /* Trim allocation. */
  boards = realloc(boards, used * sizeof(struct board));
  *boards_out = boards;
  *nboards = used;

  return error;
}

static int banko_board_mask(const struct board* board) {
  int r = 0;
  for (int row = 0; row < BOARD_ROWS; row++) {
    for (int col = 0; col < BOARD_COLS; col++) {
      r = (r << 1) | (board->cells[row][col] > 0);
    }
  }

  return r;
}

#define BOARD_ROW_PERMUTATIONS_SIZE 126
#define BANKO_NUM_MASKS 831986
#define BANKO_MASK_INDEX_BITS 20
#define BANKO_MASK_BITS 27

uint8_t row_mask_permutations[BOARD_ROW_PERMUTATIONS_SIZE][BOARD_COLS] =
  {
    {0, 0, 0, 0, 1, 1, 1, 1, 1},
    {0, 0, 0, 1, 0, 1, 1, 1, 1},
    {0, 0, 0, 1, 1, 0, 1, 1, 1},
    {0, 0, 0, 1, 1, 1, 0, 1, 1},
    {0, 0, 0, 1, 1, 1, 1, 0, 1},
    {0, 0, 0, 1, 1, 1, 1, 1, 0},
    {0, 0, 1, 0, 0, 1, 1, 1, 1},
    {0, 0, 1, 0, 1, 0, 1, 1, 1},
    {0, 0, 1, 0, 1, 1, 0, 1, 1},
    {0, 0, 1, 0, 1, 1, 1, 0, 1},
    {0, 0, 1, 0, 1, 1, 1, 1, 0},
    {0, 0, 1, 1, 0, 0, 1, 1, 1},
    {0, 0, 1, 1, 0, 1, 0, 1, 1},
    {0, 0, 1, 1, 0, 1, 1, 0, 1},
    {0, 0, 1, 1, 0, 1, 1, 1, 0},
    {0, 0, 1, 1, 1, 0, 0, 1, 1},
    {0, 0, 1, 1, 1, 0, 1, 0, 1},
    {0, 0, 1, 1, 1, 0, 1, 1, 0},
    {0, 0, 1, 1, 1, 1, 0, 0, 1},
    {0, 0, 1, 1, 1, 1, 0, 1, 0},
    {0, 0, 1, 1, 1, 1, 1, 0, 0},
    {0, 1, 0, 0, 0, 1, 1, 1, 1},
    {0, 1, 0, 0, 1, 0, 1, 1, 1},
    {0, 1, 0, 0, 1, 1, 0, 1, 1},
    {0, 1, 0, 0, 1, 1, 1, 0, 1},
    {0, 1, 0, 0, 1, 1, 1, 1, 0},
    {0, 1, 0, 1, 0, 0, 1, 1, 1},
    {0, 1, 0, 1, 0, 1, 0, 1, 1},
    {0, 1, 0, 1, 0, 1, 1, 0, 1},
    {0, 1, 0, 1, 0, 1, 1, 1, 0},
    {0, 1, 0, 1, 1, 0, 0, 1, 1},
    {0, 1, 0, 1, 1, 0, 1, 0, 1},
    {0, 1, 0, 1, 1, 0, 1, 1, 0},
    {0, 1, 0, 1, 1, 1, 0, 0, 1},
    {0, 1, 0, 1, 1, 1, 0, 1, 0},
    {0, 1, 0, 1, 1, 1, 1, 0, 0},
    {0, 1, 1, 0, 0, 0, 1, 1, 1},
    {0, 1, 1, 0, 0, 1, 0, 1, 1},
    {0, 1, 1, 0, 0, 1, 1, 0, 1},
    {0, 1, 1, 0, 0, 1, 1, 1, 0},
    {0, 1, 1, 0, 1, 0, 0, 1, 1},
    {0, 1, 1, 0, 1, 0, 1, 0, 1},
    {0, 1, 1, 0, 1, 0, 1, 1, 0},
    {0, 1, 1, 0, 1, 1, 0, 0, 1},
    {0, 1, 1, 0, 1, 1, 0, 1, 0},
    {0, 1, 1, 0, 1, 1, 1, 0, 0},
    {0, 1, 1, 1, 0, 0, 0, 1, 1},
    {0, 1, 1, 1, 0, 0, 1, 0, 1},
    {0, 1, 1, 1, 0, 0, 1, 1, 0},
    {0, 1, 1, 1, 0, 1, 0, 0, 1},
    {0, 1, 1, 1, 0, 1, 0, 1, 0},
    {0, 1, 1, 1, 0, 1, 1, 0, 0},
    {0, 1, 1, 1, 1, 0, 0, 0, 1},
    {0, 1, 1, 1, 1, 0, 0, 1, 0},
    {0, 1, 1, 1, 1, 0, 1, 0, 0},
    {0, 1, 1, 1, 1, 1, 0, 0, 0},
    {1, 0, 0, 0, 0, 1, 1, 1, 1},
    {1, 0, 0, 0, 1, 0, 1, 1, 1},
    {1, 0, 0, 0, 1, 1, 0, 1, 1},
    {1, 0, 0, 0, 1, 1, 1, 0, 1},
    {1, 0, 0, 0, 1, 1, 1, 1, 0},
    {1, 0, 0, 1, 0, 0, 1, 1, 1},
    {1, 0, 0, 1, 0, 1, 0, 1, 1},
    {1, 0, 0, 1, 0, 1, 1, 0, 1},
    {1, 0, 0, 1, 0, 1, 1, 1, 0},
    {1, 0, 0, 1, 1, 0, 0, 1, 1},
    {1, 0, 0, 1, 1, 0, 1, 0, 1},
    {1, 0, 0, 1, 1, 0, 1, 1, 0},
    {1, 0, 0, 1, 1, 1, 0, 0, 1},
    {1, 0, 0, 1, 1, 1, 0, 1, 0},
    {1, 0, 0, 1, 1, 1, 1, 0, 0},
    {1, 0, 1, 0, 0, 0, 1, 1, 1},
    {1, 0, 1, 0, 0, 1, 0, 1, 1},
    {1, 0, 1, 0, 0, 1, 1, 0, 1},
    {1, 0, 1, 0, 0, 1, 1, 1, 0},
    {1, 0, 1, 0, 1, 0, 0, 1, 1},
    {1, 0, 1, 0, 1, 0, 1, 0, 1},
    {1, 0, 1, 0, 1, 0, 1, 1, 0},
    {1, 0, 1, 0, 1, 1, 0, 0, 1},
    {1, 0, 1, 0, 1, 1, 0, 1, 0},
    {1, 0, 1, 0, 1, 1, 1, 0, 0},
    {1, 0, 1, 1, 0, 0, 0, 1, 1},
    {1, 0, 1, 1, 0, 0, 1, 0, 1},
    {1, 0, 1, 1, 0, 0, 1, 1, 0},
    {1, 0, 1, 1, 0, 1, 0, 0, 1},
    {1, 0, 1, 1, 0, 1, 0, 1, 0},
    {1, 0, 1, 1, 0, 1, 1, 0, 0},
    {1, 0, 1, 1, 1, 0, 0, 0, 1},
    {1, 0, 1, 1, 1, 0, 0, 1, 0},
    {1, 0, 1, 1, 1, 0, 1, 0, 0},
    {1, 0, 1, 1, 1, 1, 0, 0, 0},
    {1, 1, 0, 0, 0, 0, 1, 1, 1},
    {1, 1, 0, 0, 0, 1, 0, 1, 1},
    {1, 1, 0, 0, 0, 1, 1, 0, 1},
    {1, 1, 0, 0, 0, 1, 1, 1, 0},
    {1, 1, 0, 0, 1, 0, 0, 1, 1},
    {1, 1, 0, 0, 1, 0, 1, 0, 1},
    {1, 1, 0, 0, 1, 0, 1, 1, 0},
    {1, 1, 0, 0, 1, 1, 0, 0, 1},
    {1, 1, 0, 0, 1, 1, 0, 1, 0},
    {1, 1, 0, 0, 1, 1, 1, 0, 0},
    {1, 1, 0, 1, 0, 0, 0, 1, 1},
    {1, 1, 0, 1, 0, 0, 1, 0, 1},
    {1, 1, 0, 1, 0, 0, 1, 1, 0},
    {1, 1, 0, 1, 0, 1, 0, 0, 1},
    {1, 1, 0, 1, 0, 1, 0, 1, 0},
    {1, 1, 0, 1, 0, 1, 1, 0, 0},
    {1, 1, 0, 1, 1, 0, 0, 0, 1},
    {1, 1, 0, 1, 1, 0, 0, 1, 0},
    {1, 1, 0, 1, 1, 0, 1, 0, 0},
    {1, 1, 0, 1, 1, 1, 0, 0, 0},
    {1, 1, 1, 0, 0, 0, 0, 1, 1},
    {1, 1, 1, 0, 0, 0, 1, 0, 1},
    {1, 1, 1, 0, 0, 0, 1, 1, 0},
    {1, 1, 1, 0, 0, 1, 0, 0, 1},
    {1, 1, 1, 0, 0, 1, 0, 1, 0},
    {1, 1, 1, 0, 0, 1, 1, 0, 0},
    {1, 1, 1, 0, 1, 0, 0, 0, 1},
    {1, 1, 1, 0, 1, 0, 0, 1, 0},
    {1, 1, 1, 0, 1, 0, 1, 0, 0},
    {1, 1, 1, 0, 1, 1, 0, 0, 0},
    {1, 1, 1, 1, 0, 0, 0, 0, 1},
    {1, 1, 1, 1, 0, 0, 0, 1, 0},
    {1, 1, 1, 1, 0, 0, 1, 0, 0},
    {1, 1, 1, 1, 0, 1, 0, 0, 0},
    {1, 1, 1, 1, 1, 0, 0, 0, 0}
  };

static void banko_calculate_masks(int32_t masks[1<<BANKO_MASK_INDEX_BITS],
                                  int32_t mask_to_index[1<<BANKO_MASK_BITS]) {
  int mask_index = 0;

  for (int i = 0; i < BOARD_ROW_PERMUTATIONS_SIZE; i++) {
    for (int j = 0; j < BOARD_ROW_PERMUTATIONS_SIZE; j++) {
      for (int l = 0; l < BOARD_ROW_PERMUTATIONS_SIZE; l++) {
        struct board b;
        for (int p = 0; p < BOARD_COLS; p++) {
          b.cells[0][p] = row_mask_permutations[i][p];
        }
        for (int p = 0; p < BOARD_COLS; p++) {
          b.cells[1][p] = row_mask_permutations[j][p];
        }
        for (int p = 0; p < BOARD_COLS; p++) {
          b.cells[2][p] = row_mask_permutations[l][p];
        }

        int ok = 1;
        for (int j = 0; j < BOARD_COLS; j++) {
          if (b.cells[0][j] == 0 &&
              b.cells[1][j] == 0 &&
              b.cells[2][j] == 0) {
            ok = 0;
          }
        }

        if (!ok) {
          continue;
        }

        int mask = banko_board_mask(&b);
        masks[mask_index] = mask;
        mask_to_index[mask] = mask_index;
        mask_index++;
      }
    }
  }
}

static int banko_lookup_mask(int32_t mask, int row, int column) {
  return (mask >> ((BOARD_ROWS-1-row)*BOARD_COLS + (BOARD_COLS-1-column))) & 1;
}

#endif
