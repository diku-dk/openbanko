#ifndef ALGORITHM_6_H
#define ALGORITHM_6_H

#include "bankopladeformat/bankopladeformat.h"
#include "bitio.h"


#define A6_BOARD_ROW_PERMUTATIONS_BIT_SIZE 7 /* lg(126) */
#define A6_BOARD_NUMBERS_PER_ROW_SIZE 5
#define A6_ARITHMETIC_CODING_BASE (11 * 10 * 10 * 10 * 10)
#define A6_BOARD_COMPRESSED_MAX_BITS_SIZE 51

typedef uint8_t bit_t;

typedef struct {
  bit_t cells[BOARD_ROWS][BOARD_COLS];
} a6_mask_t;

typedef struct {
  bit_t cells[BOARD_ROWS][A6_BOARD_ROW_PERMUTATIONS_BIT_SIZE];
} a6_mask_compr_t;

typedef struct {
  uint8_t cells[BOARD_ROWS][A6_BOARD_NUMBERS_PER_ROW_SIZE];
} a6_board_values_t;

typedef struct {
  uint64_t cells[BOARD_ROWS];
} a6_row_values_t;


static void a6_make_board_mask(a6_mask_t *dest_mask,
                               const struct board src_board) {
  for (int row = 0; row < BOARD_ROWS; row++) {
    for (int col = 0; col < BOARD_COLS; col++) {
      dest_mask->cells[row][col] = src_board.cells[row][col] > 0;
    }
  }
}

static void a6_compress_board_mask
(
 a6_mask_compr_t *dest_compr,
 const a6_mask_t src_mask
) {
  int match;
  for (int row = 0; row < BOARD_ROWS; row++) {
    for (int i = 0; i < BOARD_ROW_PERMUTATIONS_SIZE; i++) {
      match = 1;
      for (int col = 0; col < BOARD_COLS; col++) {
        if (row_mask_permutations[i][col] != src_mask.cells[row][col]) {
          match = 0;
          break;
        }
      }
      if (match) {
        for (int j = 0; j < A6_BOARD_ROW_PERMUTATIONS_BIT_SIZE; j++) {
          dest_compr->cells[row][j] = (i >> j) & 1;
        }
        break;
      }
    }
  }
}

static void a6_uncompress_board_mask
(
 a6_mask_t *dest_mask,
 const a6_mask_compr_t src_compr
) {
  uint8_t index_temp;
  for (int row = 0; row < BOARD_ROWS; row++) {
    index_temp = 0;
    for (int i = 0; i < A6_BOARD_ROW_PERMUTATIONS_BIT_SIZE; i++) {
      index_temp |= src_compr.cells[row][i] << i;
    }
    for (int col = 0; col < BOARD_COLS; col++) {
      dest_mask->cells[row][col] = row_mask_permutations[index_temp][col];
    }
  }
}

static void a6_shorten_numbers(a6_board_values_t *dest_board_values,
                               const struct board src_board) {
  size_t index_cur[BOARD_ROWS] = {0, 0, 0};
  uint8_t offset_column = 0;
  for (int col = 0; col < BOARD_COLS; col++) {
    uint8_t offset_prev = 0;
    for (int row = 0; row < BOARD_ROWS; row++) {
      uint8_t val_cur = src_board.cells[row][col];
      if (val_cur != 0) {
        val_cur -= offset_column;
        dest_board_values->cells[row][index_cur[row]] = val_cur - offset_prev;
        index_cur[row]++;
        offset_prev = val_cur;
      }
    }
    offset_column += 10;
  }
}

static void a6_unshorten_numbers
(
 struct board *dest_board,
 const a6_board_values_t src_board_values,
 const a6_mask_t src_board_mask
) {
  uint8_t val_cur;

  uint8_t index_cur[BOARD_ROWS] = {0, 0, 0};
  uint8_t offset_column = 0;
  for (int col = 0; col < BOARD_COLS; col++) {
    uint8_t offset_prev = 0;
    for (int row = 0; row < BOARD_ROWS; row++) {
      if (src_board_mask.cells[row][col]) {
        val_cur = src_board_values.cells[row][row[index_cur]];
        index_cur[row]++;
        val_cur += offset_prev;
        dest_board->cells[row][col] = val_cur + offset_column;
        offset_prev = val_cur;
      } else {
        dest_board->cells[row][col] = 0;
      }
    }
    offset_column += 10;
  }
}

static void a6_make_row_values(a6_row_values_t *dest_row_values,
                               const a6_board_values_t src_board_values) {
  for (int row = 0; row < BOARD_ROWS; row++) {
    uint64_t val_cur = 0;
    val_cur += src_board_values.cells[row][4];
    val_cur += src_board_values.cells[row][3] * 11;
    val_cur += src_board_values.cells[row][2] * 11 * 10;
    val_cur += src_board_values.cells[row][1] * 11 * 10 * 10;
    val_cur += src_board_values.cells[row][0] * 11 * 10 * 10 * 10;
    dest_row_values->cells[row] = val_cur;
  }
}

static void a6_make_board_values(a6_board_values_t *dest_board_values,
                                 const a6_row_values_t src_row_values) {
  for (int row = 0; row < BOARD_ROWS; row++) {
    uint64_t orig = src_row_values.cells[row];
    dest_board_values->cells[row][4] = orig % 11;
    orig /= 11;
    dest_board_values->cells[row][3] = orig % 10;
    orig /= 10;
    dest_board_values->cells[row][2] = orig % 10;
    orig /= 10;
    dest_board_values->cells[row][1] = orig % 10;
    orig /= 10;
    dest_board_values->cells[row][0] = orig;
  }
}

static uint64_t a6_arithmetic_encode(const a6_row_values_t src_row_values) {
  uint64_t n = 0;
  uint64_t b = 1;
  for (int row = 0; row < BOARD_ROWS; row++) {
    n += src_row_values.cells[row] * b;
    b *= A6_ARITHMETIC_CODING_BASE;
  }
  return n;
}

static void a6_arithmetic_decode(a6_row_values_t *dest_row_values,
                                 uint64_t src_n) {
  for (int row = 0; row < BOARD_ROWS; row++) {
    dest_row_values->cells[row] = src_n % A6_ARITHMETIC_CODING_BASE;
    src_n /= A6_ARITHMETIC_CODING_BASE;
  }
}

void a6_compress(FILE *out, FILE *in) {
  struct board board;
  struct banko_reader reader;
  banko_reader_open(&reader, in);

  a6_mask_t board_mask;
  a6_mask_compr_t board_mask_compressed;
  a6_board_values_t board_values;
  a6_row_values_t row_values;
  uint64_t number_code;

  while (banko_reader_board(&reader, &board) == 0) {
    a6_make_board_mask(&board_mask, board);
    a6_compress_board_mask(&board_mask_compressed, board_mask);

    a6_shorten_numbers(&board_values, board);
    a6_make_row_values(&row_values, board_values);
    number_code = a6_arithmetic_encode(row_values);

    for (int row = 0; row < BOARD_ROWS; row++) {
      for (int i = 0; i < A6_BOARD_ROW_PERMUTATIONS_BIT_SIZE; i++) {
        write_bit(board_mask_compressed.cells[row][i], out);
      }
    }
    for (int i = 0; i < A6_BOARD_COMPRESSED_MAX_BITS_SIZE; i++) {
      write_bit((number_code >> i) & 1, out);
    }
  }

  flush_bit(out);

  banko_reader_close(&reader);
}

void a6_decompress(FILE *out, FILE *in) {
  struct board board;
  struct banko_writer writer;
  banko_writer_open(&writer, out);

  a6_mask_compr_t board_mask_compressed;
  a6_mask_t board_mask;
  a6_row_values_t row_values;
  a6_board_values_t board_values;
  uint64_t number_code;

  int bit_temp;
  uint64_t mask_temp;
  while (1) {
    for (int row = 0; row < BOARD_ROWS; row++) {
      for (int i = 0; i < A6_BOARD_ROW_PERMUTATIONS_BIT_SIZE; i++) {
        bit_temp = read_bit(in);
        if (bit_temp == EOF) {
          goto loop_exit;
        }
        board_mask_compressed.cells[row][i] = bit_temp;
      }
    }
    number_code = 0;
    for (int i = 0; i < A6_BOARD_COMPRESSED_MAX_BITS_SIZE; i++) {
      bit_temp = read_bit(in);
      if (bit_temp == EOF) {
        goto loop_exit;
      }
      mask_temp = bit_temp > 0;
      number_code |= mask_temp << i;
    }

    a6_uncompress_board_mask(&board_mask, board_mask_compressed);
    a6_arithmetic_decode(&row_values, number_code);
    a6_make_board_values(&board_values, row_values);
    a6_unshorten_numbers(&board, board_values, board_mask);

    banko_writer_board(&writer, &board);
  }
 loop_exit:

  banko_writer_close(&writer);
}

#endif
