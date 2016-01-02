#ifndef ALGORITHM_1_H
#define ALGORITHM_1_H

#include "bankopladeformat/bankopladeformat.h"

void a1_compress(FILE *out, FILE *in) {
  struct board board;
  struct banko_reader reader;
  banko_reader_open(&reader, in);

  int in_second_half = 0;
  unsigned char accum = 0;

  while (banko_reader_board(&reader, &board) == 0) {
    for (int row = 0; row < BOARD_ROWS; row++) {
      for (int col = 0; col < BOARD_COLS; col++) {
        uint8_t cell = board.cells[row*BOARD_COLS+col];
        if (in_second_half) {
          if (cell != 0) {
            accum |= cell - col*10;
          }
          fputc(accum, out);
          accum = 0;
          in_second_half = 0;
        } else {
          if (cell != 0) {
            accum = (cell - col*10) << 4;
          }
          in_second_half = 1;
        }
      }
    }
  }

  if (in_second_half) {
    fputc(accum, out);
    in_second_half = 0;
  }

  banko_reader_close(&reader);
}

void a1_decompress(FILE *out, FILE *in) {
  struct board board;
  struct banko_writer writer;
  banko_writer_open(&writer, out);

  int in_second_half = 0;
  int c;
  unsigned char accum;

  while (1) {
    for (int row = 0; row < BOARD_ROWS; row++) {
      for (int col = 0; col < BOARD_COLS; col++) {
        if (in_second_half) {
          if ((accum & 0xF) != 0) {
            board.cells[row*BOARD_COLS+col] = (accum & 0xF) + col*10;
          } else {
            board.cells[row*BOARD_COLS+col] = 0;
          }
          in_second_half = 0;
        } else {
          accum = fgetc(in);
          if (((accum >> 4) & 0xF) != 0) {
            board.cells[row*BOARD_COLS+col] = ((accum >> 4) & 0xF) + col*10;
          } else {
            board.cells[row*BOARD_COLS+col] = 0;
          }
          in_second_half = 1;
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
