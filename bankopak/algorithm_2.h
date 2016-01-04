#ifndef ALGORITHM_2_H
#define ALGORITHM_2_H

#include <stdint.h>
#include <error.h>

#include "bankopladeformat/bankopladeformat.h"
#include "bitio.h"

/* This means an index fits in 9 bits. */
#define A2_TABLE_SIZE 512

uint8_t a2_column_table[A2_TABLE_SIZE][BOARD_ROWS];

void a2_build_table() {
  int index = 0;

  /* Enumerate all possible columns. */
  for (int x1 = 0; x1 < 11; x1++) {
    for (int x2 = 0; x2 < 11-x1; x2++) {
      for (int x3 = 0; x3 < 11-x1-x2; x3++) {
        a2_column_table[index][0] = x1;
        a2_column_table[index][1] = x2;
        a2_column_table[index][2] = x3;
        index++;
      }
    }
  }
}

int a2_find_table_index(uint8_t column[BOARD_ROWS]) {
  for (int i = 0; i < A2_TABLE_SIZE; i++) {
    if (memcmp(column, a2_column_table[i], BOARD_ROWS) == 0) {
      return i;
    }
  }
  error(1, 0, "Cannot find column index when encoding.");
  return -1;
}

void a2_write_9bit(unsigned int c, FILE *out) {
  return write_bits(9, c, out);
}

unsigned int a2_read_9bit(FILE *in) {
  return read_bits(9, in);
}

void a2_compress(FILE *out, FILE *in) {
  struct board board;
  struct banko_reader reader;
  banko_reader_open(&reader, in);

  a2_build_table();

  while (banko_reader_board(&reader, &board) == 0) {
    for (int col = 0; col < BOARD_COLS; col++) {
      uint8_t column[BOARD_ROWS];
      uint8_t prev = col*10;
      for (int row = 0; row < BOARD_ROWS; row++) {
        uint8_t cell = board.cells[row][col];
        if (cell == 0) {
          column[row] = 0;
        } else {
          column[row] = cell - prev;
          prev = cell;
        }
      }
      a2_write_9bit(a2_find_table_index(column), out);
    }
  }

  flush_bit(out);
  banko_reader_close(&reader);
}

void a2_decompress(FILE *out, FILE *in) {
  struct board board;
  struct banko_writer writer;
  banko_writer_open(&writer, out);

  a2_build_table();

  while (1) {
    for (int col = 0; col < BOARD_COLS; col++) {
      int table_index = a2_read_9bit(in);
      if (table_index == EOF) {
        goto end;
      }

      uint8_t prev = col * 10;
      for (int row = 0; row < BOARD_ROWS; row++) {
        int cell = a2_column_table[table_index][row];
        if (cell == 0) {
          board.cells[row][col] = 0;
        } else {
          prev = board.cells[row][col] = cell + prev;
        }
      }
    }
    banko_writer_board(&writer, &board);
  }
 end:

  banko_writer_close(&writer);
}

#endif
