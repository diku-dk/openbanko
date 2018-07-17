#ifndef ALGORITHM_4_H
#define ALGORITHM_4_H

#include "bankopladeformat/bankopladeformat.h"

/* Precalculated worst case.  Actually 97 bits, which is unfortunately one bit
   above 12 bytes. */
#define NECESSARY_BYTES 13

static unsigned __int128 a4_arithmetic_encode
(
 const uint8_t *src,
 size_t src_len,
 size_t src_base
)
{
  unsigned __int128 num = 0; /* gcc extension; good enough for now */
  unsigned __int128 base_cur = 1;
  for (size_t i = 0; i < src_len; i++) {
    num += src[i] * base_cur;
    base_cur *= src_base;
  }
  return num;
}

static void a4_arithmetic_decode
(
 uint8_t *dest,
 size_t dest_len,
 size_t dest_base,
 unsigned __int128 src
)
{
  for (size_t i = 0; i < dest_len; i++) {
    dest[i] = src % dest_base;
    src /= dest_base;
  }
}

void a4_compress(FILE *out, FILE *in) {
  struct board board;
  struct banko_reader reader;
  banko_reader_open(&reader, in);

  uint8_t board_relative_values[BOARD_COLS][BOARD_ROWS];

  uint8_t value_cur;
  uint8_t offset_column;
  uint8_t offset_prev;
  unsigned __int128 number_encoding;

  while (banko_reader_board(&reader, &board) == 0) {
    offset_column = 0;
    for (int col = 0; col < BOARD_COLS; col++) {
      offset_prev = 0;
      for (int row = 0; row < BOARD_ROWS; row++) {
        value_cur = board.cells[row][col];
        if (value_cur == 0) {
          board_relative_values[col][row] = 0;
        } else {
          value_cur -= offset_column;
          value_cur += 1;
          board_relative_values[col][row] = value_cur - offset_prev;
          offset_prev = value_cur;
        }
      }
      offset_column += 10;
    }

    number_encoding = a4_arithmetic_encode((uint8_t*) board_relative_values,
                                           BOARD_ROWS * BOARD_COLS,
                                           12);

    fwrite((uint8_t*) &number_encoding, NECESSARY_BYTES, 1, out);
  }

  banko_reader_close(&reader);
}

void a4_decompress(FILE *out, FILE *in) {
  struct board board;
  struct banko_writer writer;
  banko_writer_open(&writer, out);

  unsigned __int128 number_encoding;
  uint8_t board_relative_values[BOARD_COLS][BOARD_ROWS];

  uint8_t value_cur;
  uint8_t offset_column;
  uint8_t offset_prev;

  while (1) {
    number_encoding = 0;
    if (fread((uint8_t*) &number_encoding, NECESSARY_BYTES, 1, in) != 1) {
      break;
    }

    a4_arithmetic_decode((uint8_t*) board_relative_values,
                         BOARD_ROWS * BOARD_COLS,
                         12,
                         number_encoding);

    offset_column = 0;
    for (int col = 0; col < BOARD_COLS; col++) {
      offset_prev = 0;
      for (int row = 0; row < BOARD_ROWS; row++) {
        value_cur = board_relative_values[col][row];
        if (value_cur == 0) {
          board.cells[row][col] = 0;
        } else {
          value_cur += offset_prev;
          offset_prev = value_cur;
          board.cells[row][col] = value_cur + offset_column - 1;
        }
      }
      offset_column += 10;
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
