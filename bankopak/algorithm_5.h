/*
  This is some Huffman-coding stuff, based on the assumption that it's
  good to encode 0 cheaply.  Stores the board in column-major order.
 */

#ifndef ALGORITHM_5_H
#define ALGORITHM_5_H

#include "bankopladeformat/bankopladeformat.h"
#include "bitio.h"


unsigned int a5_read_next(FILE *in) {
  if (read_bit(in) == 0) {
    return 0;
  } else {
    if (read_bit(in) == 0) {
      if (read_bit(in) == 0) {
        if (read_bit(in) == 0) {
          return 1;
        } else {
          return 2;
        }
      } else {
        if (read_bit(in) == 0) {
          return 3;
        } else {
          return 4;
        }
      }
    } else {
      if (read_bit(in) == 0) {
        if (read_bit(in) == 0) {
          return 5;
        } else {
          return 6;
        }
      } else {
        if (read_bit(in) == 0) {
          return 7;
        } else {
          if (read_bit(in) == 0) {
            return 8;
          } else {
            if (read_bit(in) == 0) {
              return 9;
            } else {
              return 10;
            }
          }
        }
      }
    }
  }
}

void a5_write_next(unsigned int c, FILE *out) {
  switch (c) {
  case 0:
    write_bit(0, out);
    return;
  case 1:
    write_bit(1, out); write_bit(0, out); write_bit(0, out); write_bit(0, out);
    return;
  case 2:
    write_bit(1, out); write_bit(0, out); write_bit(0, out); write_bit(1, out);
    return;
  case 3:
    write_bit(1, out); write_bit(0, out); write_bit(1, out); write_bit(0, out);
    return;
  case 4:
    write_bit(1, out); write_bit(0, out); write_bit(1, out); write_bit(1, out);
    return;
  case 5:
    write_bit(1, out); write_bit(1, out); write_bit(0, out); write_bit(0, out);
    return;
  case 6:
    write_bit(1, out); write_bit(1, out); write_bit(0, out); write_bit(1, out);
    return;
  case 7:
    write_bit(1, out); write_bit(1, out); write_bit(1, out); write_bit(0, out);
    return;
  case 8:
    write_bit(1, out); write_bit(1, out); write_bit(1, out); write_bit(1, out); write_bit(0, out);
    return;
  case 9:
    write_bit(1, out); write_bit(1, out); write_bit(1, out); write_bit(1, out); write_bit(1, out); write_bit(0, out);
    return;
  case 10:
    write_bit(1, out); write_bit(1, out); write_bit(1, out); write_bit(1, out); write_bit(1, out); write_bit(1, out);
    return;
  }
}

void a5_compress(FILE *out, FILE *in) {
  struct board board;
  struct banko_reader reader;
  banko_reader_open(&reader, in);

  while (banko_reader_board(&reader, &board) == 0) {
    for (int col = 0; col < BOARD_COLS; col++) {
      uint8_t prev = col*10;
      for (int row = 0; row < BOARD_ROWS; row++) {
        uint8_t cell = board.cells[row][col];
        if (cell == 0) {
          a5_write_next(0, out);
        } else {
          a5_write_next(cell - prev, out);
          prev = cell;
        }
      }
    }
  }

  flush_bit(out);

  banko_reader_close(&reader);
}

void a5_decompress(FILE *out, FILE *in) {
  struct board board;
  struct banko_writer writer;
  banko_writer_open(&writer, out);

  while (1) {
    for (int col = 0; col < BOARD_COLS; col++) {
      uint8_t prev = col*10;
      for (int row = 0; row < BOARD_ROWS; row++) {
        if (peek_bit(in) == EOF) {
          goto end;
        }

        uint8_t cell = a5_read_next(in);
        if (cell == 0) {
          board.cells[row][col] = cell;
        } else {
          prev = board.cells[row][col] = prev + cell;
        }
      }
    }

    banko_writer_board(&writer, &board);
  }
 end:

  banko_writer_close(&writer);
}

#endif
