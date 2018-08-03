// Construct the average banko board!

#include <stdlib.h>
#include <stdio.h>
#include <error.h>
#include <stdint.h>

#include "bankopladeformat.h"

int main(int argc, char** argv) {
  argv=argv;
  if (argc != 1) {
    error(1, 0, "This program does not take any options.");
  }

  struct banko_reader r;
  struct board b;

  double sums[BOARD_ROWS][BOARD_COLS] = { 0 };
  int counts[BOARD_ROWS][BOARD_COLS] = { 0 };

  banko_reader_open(&r, stdin);
  while (banko_reader_board(&r, &b) == 0) {
    for (int col = 0; col < BOARD_COLS; col++) {
      for (int row = 0; row < BOARD_ROWS; row++) {
        if (b.cells[row][col] != 0) {
          sums[row][col] += b.cells[row][col];
          counts[row][col]++;
        }
      }
    }
  }
  banko_reader_close(&r);

  struct banko_writer w;
  banko_writer_open(&w, stdout);
  for (int col = 0; col < BOARD_COLS; col++) {
    for (int row = 0; row < BOARD_ROWS; row++) {
      if (counts[row][col] > 0) {
        b.cells[row][col] = sums[row][col] / counts[row][col];
      } else {
        b.cells[row][col] = 0;
      }
    }
  }

  banko_writer_board(&w, &b);

  banko_writer_close(&w);

  return 0;
}
