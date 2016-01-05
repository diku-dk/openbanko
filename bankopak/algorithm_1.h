#ifndef ALGORITHM_1_H
#define ALGORITHM_1_H

#include "bankopladeformat/bankopladeformat.h"
#include "bitio.h"

void a1_write_4bit(unsigned int c, FILE *out) {
  return write_bits(4, c, out);
}

unsigned int a1_read_4bit(FILE *in) {
  return read_bits(4, in);
}

void a1_compress(FILE *out, FILE *in) {
  struct board board;
  struct banko_reader reader;
  banko_reader_open(&reader, in);

  while (banko_reader_board(&reader, &board) == 0) {
    for (int row = 0; row < BOARD_ROWS; row++) {
      for (int col = 0; col < BOARD_COLS; col++) {
        uint8_t cell = board.cells[row][col];
        if (cell == 0) {
          a1_write_4bit(0, out);
        } else {
          a1_write_4bit(cell - col*10, out);
        }
      }
    }
  }

  flush_bit(out);

  banko_reader_close(&reader);
}

void a1_decompress(FILE *out, FILE *in) {
  struct board board;
  struct banko_writer writer;
  banko_writer_open(&writer, out);

  int c;

  while (1) {
    for (int row = 0; row < BOARD_ROWS; row++) {
      for (int col = 0; col < BOARD_COLS; col++) {
        uint8_t cell = a1_read_4bit(in);
        if (cell != 0) {
          board.cells[row][col] = cell + col*10;
        } else {
          board.cells[row][col] = 0;
        }
      }
    }

    banko_writer_board(&writer, &board);

    c = fgetc(in);
    ungetc(c, in);
    if (c == EOF) {
      break;
    }
  }

  banko_writer_close(&writer);
}

#endif
