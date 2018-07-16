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
  writer = writer;
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

#endif
