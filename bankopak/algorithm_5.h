/*
  This is some Huffman-coding stuff, based on the assumption that it's
  good to encode 0 cheaply.  Stores the board in column-major order.
 */

#ifndef ALGORITHM_5_H
#define ALGORITHM_5_H

#include "bankopladeformat/bankopladeformat.h"
#include "bitio.h"

unsigned int a5_huffman_decode(FILE *in, size_t maplen, const char** map_in) {
  char **map = malloc(sizeof(const char*) * maplen);
  memcpy(map, map_in, sizeof(const char*) * maplen);

  while (1) {
    int b = read_bit(in);

    if (b == EOF) {
      free(map);
      return EOF;
    }

    int hit, hits = 0;
    for (size_t i = 0; i < maplen; i++) {
      if (map[i] == NULL) {
        continue;
      }

      if (b + '0' == map[i][0]) {
        map[i]++;
        hit = i;
        hits++;
      } else {
        map[i] = NULL;
      }
    }
    if (hits == 1) {
      free(map);
      return hit;
    }
  }
}

void a5_huffman_encode(unsigned int c, FILE *out, const char** map) {
  const char *s = map[c];

  while (*s) {
    write_bit(*s - '0', out);
    s++;
  }
}

const char* huffman_table_default[] =
  {
    "0",
    "1000",
    "1001",
    "1010",
    "1011",
    "1100",
    "1101",
    "1110",
    "11110",
    "111110",
    "1111110",
    "1111111",
  };

const char* huffman_table_notfirst[] =
  {
    "0",
    "1000",
    "1001",
    "1010",
    "1011",
    "1100",
    "1101",
    "1110",
    "11110",
    "111110",
    "111111",
  };

unsigned int a5_read_next(int row, int col, int prev, int hasprev, FILE *in) {
  row = row;
  col = col;
  prev = prev;
  if (hasprev) {
    return a5_huffman_decode(in, 12, huffman_table_notfirst);
  } else {
    return a5_huffman_decode(in, 12, huffman_table_default);
  }
}

void a5_write_next(int row, int col, int prev, int hasprev, unsigned int c, FILE *out) {
  row = row;
  col = col;
  prev = prev;
  if (hasprev) {
    return a5_huffman_encode(c, out, huffman_table_notfirst);
  } else {
    return a5_huffman_encode(c, out, huffman_table_default);
  }
}

void a5_compress(FILE *out, FILE *in) {
  struct board board;
  struct banko_reader reader;
  banko_reader_open(&reader, in);

  while (banko_reader_board(&reader, &board) == 0) {
    for (int col = 0; col < BOARD_COLS; col++) {
      uint8_t prev = col*10;
      int hasprev = 0;
      for (int row = 0; row < BOARD_ROWS; row++) {
        uint8_t cell = board.cells[row][col];
        if (cell == 0) {
          a5_write_next(row, col, prev, hasprev, 0, out);
        } else {
          a5_write_next(row, col, prev, hasprev, cell - prev + 1, out);
          prev = cell;
          hasprev = 1;
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
      int hasprev = 0;
      for (int row = 0; row < BOARD_ROWS; row++) {
        if (peek_bit(in) == EOF) {
          goto end;
        }

        uint8_t cell = a5_read_next(row, col, prev, hasprev, in);
        if (cell == 0) {
          board.cells[row][col] = cell;
        } else {
          prev = board.cells[row][col] = prev + cell - 1;
          hasprev = 1;
        }
      }
    }

    banko_writer_board(&writer, &board);
  }
 end:

  banko_writer_close(&writer);
}

#endif
