#ifndef ALGORITHM_7_H
#define ALGORITHM_7_H

#include <string.h>
#include <assert.h>
#include "bankopladeformat/bankopladeformat.h"
#include "bitio.h"
#include "algorithm_6.h"

// Mask is least significant 27 bits.
int32_t masks[126*126*126] = { 0 };
int32_t mask_to_index[1<<27];

static void calculate_masks() {
  for (int i = 0; i < BOARD_ROW_PERMUTATIONS_SIZE; i++) {
    for (int j = 0; j < BOARD_ROW_PERMUTATIONS_SIZE; j++) {
      for (int l = 0; l < BOARD_ROW_PERMUTATIONS_SIZE; l++) {
        struct board b;
        for (int p = 0; p < BOARD_COLS; p++) {
          b.cells[0][p] = a6_row_mask_permutations[i][p];
        }
        for (int p = 0; p < BOARD_COLS; p++) {
          b.cells[1][p] = a6_row_mask_permutations[j][p];
        }
        for (int p = 0; p < BOARD_COLS; p++) {
          b.cells[2][p] = a6_row_mask_permutations[l][p];
        }

        int mask = banko_board_mask(&b);
        int index =
          i*BOARD_ROW_PERMUTATIONS_SIZE*BOARD_ROW_PERMUTATIONS_SIZE +
          j*BOARD_ROW_PERMUTATIONS_SIZE +
          l;
        masks[index] = mask;
        mask_to_index[mask] = index;
      }
    }
  }
}

typedef uint8_t bit_t;

static int a7_make_board_mask_index(const struct board src_board) {
  return mask_to_index[banko_board_mask(&src_board)];
}

void a7_compress(FILE *out, FILE *in) {
  calculate_masks();

  struct board board;
  struct banko_reader reader;
  banko_reader_open(&reader, in);

  a6_mask_t board_mask;
  a6_mask_compr_t board_mask_compressed;
  a6_board_values_t board_values;
  a6_row_values_t row_values;
  uint64_t number_code;
  int cur_mask = -1;

  while (banko_reader_board(&reader, &board) == 0) {
    a6_make_board_mask(&board_mask, board);
    a6_compress_board_mask(&board_mask_compressed, board_mask);

    a6_shorten_numbers(&board_values, board);
    a6_make_row_values(&row_values, board_values);
    number_code = a6_arithmetic_encode(row_values);

    int mask_id = a7_make_board_mask_index(board);
    if (mask_id == cur_mask) {
      write_bit(0, out);
    } else {
      write_bit(1, out);
      cur_mask = mask_id;
      for (int row = 0; row < BOARD_ROWS; row++) {
        for (int i = 0; i < BOARD_ROW_PERMUTATIONS_BIT_SIZE; i++) {
          write_bit(board_mask_compressed.cells[row][i], out);
        }
      }
    }

    for (int i = 0; i < A6_BOARD_COMPRESSED_MAX_BITS_SIZE; i++) {
      write_bit((number_code >> i) & 1, out);
    }
  }

  flush_bit(out);

  banko_reader_close(&reader);
}

void a7_decompress(FILE *out, FILE *in) {
  calculate_masks();

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
    if (read_bit(in) == 1) {
      for (int row = 0; row < BOARD_ROWS; row++) {
        for (int i = 0; i < BOARD_ROW_PERMUTATIONS_BIT_SIZE; i++) {
          bit_temp = read_bit(in);
          if (bit_temp == EOF) {
            goto loop_exit;
          }
          board_mask_compressed.cells[row][i] = bit_temp;
        }
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
