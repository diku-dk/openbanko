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

/*
 * All function definitions inlined and defined statically.
 */

static void banko_writer_open(struct banko_writer *writer, FILE *file) {
  writer->file = file;
  writer->first = 1;

  fputs("[\n", writer->file);
}

static void banko_writer_board(struct banko_writer *writer, const struct board *board) {
  if (!writer->first) {
    fputs(",\n", writer->file);
  } else {
    writer->first = 0;
  }

  fputs("[", writer->file);
  for (int row = 0; row < BOARD_ROWS; row++) {
    if (row > 0) {
      fputs(" ", writer->file);
    }
    fputs("[", writer->file);

    for (int col = 0; col < BOARD_COLS; col++) {
      fprintf(writer->file, "%2d", board->cells[row][col]);
      if (col != BOARD_COLS-1) {
        fputs(", ", writer->file);
      }
    }
    if (row != BOARD_ROWS-1) {
      fputs("],\n", writer->file);
    } else {
      fputs("]]", writer->file);
    }
  }
}

static void banko_writer_close(struct banko_writer *writer) {
  fputs("\n]\n", writer->file);
}

static void skipspaces(struct banko_reader *reader) {
  int c = fgetc(reader->file);

  if (isspace(c)) {
    skipspaces(reader);
  } else if (c != EOF) {
    ungetc(c, reader->file);
  }
}

static void expect_char(struct banko_reader *reader, char expects) {
  char got = fgetc(reader->file);
  if (got == expects) {
    return;
  } else {
    reader->error = 1;
  }
}

static int banko_reader_open(struct banko_reader *reader, FILE *file) {
  reader->file = file;
  reader->error = 0;
  reader->first = 1;

  skipspaces(reader);
  expect_char(reader, '[');

  return reader->error;
}

static int banko_reader_board(struct banko_reader *reader, struct board *board) {
  if (reader->first) {
    skipspaces(reader);
    reader->first = 0;
  } else {
    expect_char(reader, ',');
  }

  skipspaces(reader);
  expect_char(reader, '[');

  /* Read row-by-row. */
  for (int row = 0; row < BOARD_ROWS; row++) {
    if (row > 0) {
      skipspaces(reader);
      expect_char(reader, ',');
    }

    skipspaces(reader);
    expect_char(reader, '[');

    for (int col = 0; col < BOARD_COLS; col++) {
      if (col > 0) {
        skipspaces(reader);
        expect_char(reader, ',');
      }

      skipspaces(reader);
      int x = 0;
      if (fscanf(reader->file, "%d", &x) != 1 || x < 0 || x > 90) {
        reader->error = 1;
      } else {
        board->cells[row][col] = x;
      }
    }

    skipspaces(reader);
    expect_char(reader, ']');
  }

  skipspaces(reader);
  expect_char(reader, ']');

  return reader->error;
}

static int banko_reader_close(struct banko_reader *reader) {
  skipspaces(reader);
  expect_char(reader, ']');
  return reader->error;
}

#endif
