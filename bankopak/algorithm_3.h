#ifndef ALGORITHM_3_H
#define ALGORITHM_3_H

#include "bankopladeformat/bankopladeformat.h"

void a3_compress(FILE *out, FILE *in) {
  struct board board;
  struct banko_reader reader;
  banko_reader_open(&reader, in);

  uint32_t board_layout;
  uint8_t board_values[15];

  while (banko_reader_board(&reader, &board) == 0) {
    board_layout = 0;
    uint32_t mask = 1;
    int index_cur = 0;
    for (int row = 0; row < BOARD_ROWS; row++) {
      for (int col = 0; col < BOARD_COLS; col++) {
        if (board.cells[row][col] != 0) {
          board_layout |= mask;
          board_values[index_cur] = board.cells[row][col];
          index_cur++;
        }
        mask <<= 1;
      }
    }

    for (int i = 0; i < 32; i += 8) {
      fputc((unsigned char) (board_layout >> i), out);
    }
    for (int i = 0; i < 15; i++) {
      fputc(board_values[i], out);
    }
  }

  banko_reader_close(&reader);
}

void a3_decompress(FILE *out, FILE *in) {
  struct board board;
  struct banko_writer writer;
  banko_writer_open(&writer, out);

  /* We only need 27 bits, but the wasted 5 bits aren't a big deal. */
  uint32_t board_layout;

  uint8_t board_values[15];

  while (1) {
    if ((fread(&board_layout, 4, 1, in) != 1) ||
        (fread(board_values, 1, 15, in) != 15)) {
      break;
    }

    uint32_t mask = 1;
    int index_cur = 0;
    uint8_t val_cur;
    for (int row = 0; row < BOARD_ROWS; row++) {
      for (int col = 0; col < BOARD_COLS; col++) {
        if (board_layout & mask) {
          val_cur = board_values[index_cur];
          index_cur++;
        } else {
          val_cur = 0;
        }
        board.cells[row][col] = val_cur;
        mask <<= 1;
      }
    }

    banko_writer_board(&writer, &board);

    int c = fgetc(in);
    ungetc(c, in);
    if (c == EOF) {
      break;
    }
  }

  banko_writer_close(&writer);
}

#endif
