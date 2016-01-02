#ifndef ALGORITHM_0_H
#define ALGORITHM_0_H

#include "bankopladeformat/bankopladeformat.h"

void a0_compress(FILE *out, FILE *in) {
  struct board board;
  struct banko_reader reader;
  banko_reader_open(&reader, in);

  while (banko_reader_board(&reader, &board) == 0) {
    for (int row = 0; row < BOARD_ROWS; row++) {
      for (int col = 0; col < BOARD_COLS; col++) {
        fputc(board.cells[row][col], out);
      }
    }
  }

  banko_reader_close(&reader);
}

void a0_decompress(FILE *out, FILE *in) {
  struct board board;
  struct banko_writer writer;
  banko_writer_open(&writer, out);

  fread(board.cells, sizeof(board.cells[0]), BOARD_ROWS*BOARD_COLS, in);
  banko_writer_board(&writer, &board);

  banko_writer_close(&writer);
}

#endif
