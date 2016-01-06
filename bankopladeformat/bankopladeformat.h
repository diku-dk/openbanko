#ifndef BANKOPLADEFORMAT_H
#define BANKOPLADEFORMAT_H

#include <stdio.h>
#include <stdint.h>
#include <ctype.h>

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
static void banko_reader_clear_error(struct banko_reader *reader);

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


#endif
