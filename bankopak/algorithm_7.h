#ifndef ALGORITHM_7_H
#define ALGORITHM_7_H

#include <string.h>
#include <assert.h>
#include "bankopladeformat/bankopladeformat.h"
#include "bitio.h"
#include "algorithm_6.h"

// Mask is least significant 27 bits.
int32_t masks[1<<BANKO_MASK_INDEX_BITS] = { 0 };
int32_t mask_to_index[1<<BANKO_MASK_BITS] = { -1 };

static int a7_make_board_mask_index(const struct board src_board) {
  return mask_to_index[banko_board_mask(&src_board)];
}

static a6_mask_t a7_mask_index_to_mask(int index) {
  int mask = masks[index];
  a6_mask_t a6_mask;

  for (int i = 0; i < BOARD_ROWS; i++) {
    for (int j = 0; j < BOARD_COLS; j++) {
      a6_mask.cells[i][j] = (mask >> ((BOARD_ROWS-1-i)*BOARD_COLS + (BOARD_COLS-1-j))) & 1;
    }
  }

  return a6_mask;
}

static void a7_write_mask_index(FILE *out, int delta, int new_id) {
  if (delta < 0) {
    write_bit(1, out);
    write_bits(A7_MASK_INDEX_BITS, new_id, out);
  } else if (delta < 8) {
    write_bit(0, out);
    write_bits(3, delta, out);
  } else {
    write_bit(1, out);
    write_bits(A7_MASK_INDEX_BITS, new_id, out);
  }
}

static int a7_read_mask_index(FILE *in, int cur_mask_id) {
  if (read_bit(in) == 1) {
    return read_bits(A7_MASK_INDEX_BITS, in);
  } else {
    return cur_mask_id + read_bits(3, in);
  }
}

void a7_compress(FILE *out, FILE *in) {
  banko_calculate_masks(masks, mask_to_index);

  struct board board;
  struct banko_reader reader;
  banko_reader_open(&reader, in);

  a6_mask_t board_mask;
  a6_board_values_t board_values;
  a6_row_values_t row_values;
  uint64_t number_code;
  int cur_mask = 0;

  while (banko_reader_board(&reader, &board) == 0) {
    a6_make_board_mask(&board_mask, board);

    a6_shorten_numbers(&board_values, board);
    a6_make_row_values(&row_values, board_values);
    number_code = a6_arithmetic_encode(row_values);

    int mask_id = a7_make_board_mask_index(board);
    if (mask_id == cur_mask) {
      write_bit(0, out);
    } else {
      write_bit(1, out);
      a7_write_mask_index(out, mask_id - cur_mask, mask_id);
      cur_mask = mask_id;
    }

    for (int i = 0; i < A6_BOARD_COMPRESSED_MAX_BITS_SIZE; i++) {
      write_bit((number_code >> i) & 1, out);
    }
  }

  flush_bit(out);

  banko_reader_close(&reader);
}

void a7_decompress(FILE *out, FILE *in) {
  banko_calculate_masks(masks, mask_to_index);

  struct board board;
  struct banko_writer writer;
  banko_writer_open(&writer, out);

  a6_mask_t board_mask;
  a6_row_values_t row_values;
  a6_board_values_t board_values;
  uint64_t number_code;

  int bit_temp;
  uint64_t mask_temp;
  int cur_mask_id = 0;
  board_mask = a7_mask_index_to_mask(cur_mask_id);

  while (1) {
    if (read_bit(in) == 1) {
      cur_mask_id = a7_read_mask_index(in, cur_mask_id);
      board_mask = a7_mask_index_to_mask(cur_mask_id);
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

    a6_arithmetic_decode(&row_values, number_code);
    a6_make_board_values(&board_values, row_values);
    a6_unshorten_numbers(&board, board_values, board_mask);

    banko_writer_board(&writer, &board);
  }
 loop_exit:

  banko_writer_close(&writer);
}

#endif
